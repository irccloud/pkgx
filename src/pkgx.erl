-module(pkgx).
-export([main/1]).

main(CmdLine) ->
    OptSpecList = option_spec_list(),

    case getopt:parse(OptSpecList, CmdLine) of
        {ok, {Options, _NonOptArgs}} ->
            case proplists:get_value(help, Options) of
                true ->
                    getopt:usage(OptSpecList, "pkgx");
                undefined ->
                    makepackages(Options)
                end;
        {error, {Reason, Data}} ->
            io:format("Error: ~s ~p~n~n", [Reason, Data]),
            getopt:usage(OptSpecList, "pkgx")
    end.

option_spec_list() ->
    CurrentUser = os:getenv("USER"),
    [
     {help,     $?,     "help",     undefined,              "Show the program options"},
     {author,   $a,     "author",   {string, CurrentUser},  "Package author"},
     {email,    $e,     "email",    {string, CurrentUser ++ "@localhost"}, "Email address of the package author"},
     {usergroup,$U,     "ug",       {string, "root:root"},  "User:Group that should own the package contents"},
     {output,   $o,     "output",   {string, "./packages"}, "Directory where the packages will be output"},
     {relpath,  $p,     "relpath",  {string, "./_build/$ver/rel/$relname"}, "The path to the releases dir"},
     {relname,  $n,     "relname",  string,                 "The release name you gave relx"},
     {buildver, $b,     "buildver", string,                 "The version to build"},
     {upfrom,   $u,     "upfrom",   string,                 "The version to upgrade from"},
     {confirm,  $c,     "confirm",  {boolean, true},        "Confirm build settings before proceeding"}
    ].

makepackages(Options) ->
    ok = application:load(erlydtl),
    ok = application:load(pkgx),

    RelPath = proplists:get_value(relpath, Options, undefined),
    ReleasesFile = "/releases/RELEASES",

    {ok, [ReleasesList]} = file:consult(RelPath ++ ReleasesFile),
    [Release|_] = lists:sort(ReleasesList),
    {release, AppName, ReleaseVsn, ErtsVsn, Deps, _State} = Release,

    io:format(user, "Using release: ~s ~s~n", [AppName, ReleaseVsn]),

    Releases = build_release_history(RelPath, AppName, ReleaseVsn),
    {ParentReleaseVsn, ParentDeps, _GrandparentDeps} = get_versions_to_replace(Releases),

    InstallLocation = "/opt/" ++ AppName,
    OutputPath = proplists:get_value(output, Options) ++ "/",
    file:make_dir(OutputPath),

    ErtsDep = [{erts, ErtsVsn, "erts-" ++ ErtsVsn}],
    make_dep_packages(Options, AppName, ErtsDep, [], ParentDeps, InstallLocation, OutputPath),

    InstallPrefix = InstallLocation ++  "/lib",
    make_dep_packages(Options, AppName, Deps, ErtsDep, ParentDeps, InstallPrefix, OutputPath),

    make_release_package(Options, AppName, ReleaseVsn, ParentReleaseVsn, ErtsVsn, Deps ++ ErtsDep, ParentDeps, InstallLocation, OutputPath),

    make_meta_package(Options, AppName, ReleaseVsn, ParentReleaseVsn, Deps ++ ErtsDep, ParentDeps, InstallLocation, OutputPath).


get_versions_to_replace(Releases) when length(Releases) > 2 ->
    {ParentVsn,_,ParentErtsVsn,ParentDeps} = lists:nth(2, Releases),
    {_,_,GrandparentErtsVsn,GrandparentDeps} = lists:nth(3, Releases),
    Depends = ParentDeps ++ [ParentErtsVsn],
    Replaces = GrandparentDeps ++ [GrandparentErtsVsn],
    {ParentVsn, Depends, Replaces};
get_versions_to_replace(Releases) when length(Releases) =:= 2 ->
    {ParentVsn,_,ParentErtsVsn,ParentDeps} = lists:nth(2, Releases),
    Depends = ParentDeps ++ [ParentErtsVsn],
    {ParentVsn, Depends, []};
get_versions_to_replace(_Releases) ->
    {undefined, [], []}.
    

build_release_history(RelPath, AppName, RelVersion) ->
    build_release_history(RelPath, AppName, RelVersion, []).

build_release_history(_RelPath, _AppName, undefined, Releases) ->
    lists:reverse(Releases);
build_release_history(RelPath, AppName, RelVersion, Releases) ->
    PreviousVersion = case file:consult(RelPath ++ "/releases/" ++ RelVersion ++ "/relup") of
        {ok,[{_,[{Previously,_,_}],_}]} -> 
            Previously;
        {error, _} ->
            undefined
    end,

    {ok, [{release, _, ErtsVersion, Deps}]} = file:consult(RelPath ++ "/releases/" ++ RelVersion ++ "/" ++ AppName ++ ".rel"),
    build_release_history(RelPath, AppName, PreviousVersion, [{RelVersion, PreviousVersion, ErtsVersion, Deps}|Releases]).


dep_to_packagename(AppName, DepNameList, DepVersion) ->
    CompatDepName = re:replace(DepNameList, "_", "-", [global, {return, list}]),
    AppName ++ "-" ++ CompatDepName ++ "-" ++ DepVersion.


make_dep_packages(BaseVars, AppName, [Dep|Deps], SubDeps, ParentDeps, InstallPrefix, OutputPath) ->
    {DepName, DepVersion, DepPath} = Dep,
    DepNameList = atom_to_list(DepName),
    PackageName = dep_to_packagename(AppName, DepNameList, DepVersion),

    ParentVersion = proplists:get_value(DepName, ParentDeps, undefined),
    ExtraTemplates = case DepVersion /= ParentVersion andalso ParentVersion /= undefined of
        true ->
            [
                {"debian/preinst", deb_debian_preinst_dtl},
                {"debian/postinst", deb_debian_postinst_dtl}
            ];
        false ->
            [
                {"debian/postinst", deb_debian_postinst_dtl}
            ]
    end,

    AppPath = DepPath ++ "/ebin/" ++ DepNameList ++ ".app",
    Description = case filelib:is_file(AppPath) of
        true ->
            {ok, [{application, _, AppProperties}]} = file:consult(AppPath),
            proplists:get_value(description, AppProperties, DepNameList);
        false ->
            DepNameList
    end,

    RelPath = proplists:get_value(relpath, BaseVars, undefined),

    DepList     = compile_dep_list(AppName, SubDeps, []),
    DepString   = string:join(DepList, ", "),

    Vars = BaseVars ++ [
        {install_prefix, InstallPrefix}, 
        {install_dir_name, DepNameList ++ "-" ++ DepVersion}, 
        {app, DepName}, 
        {package_name, PackageName}, 
        {version, "1"}, 
        {dep_version, DepVersion}, 
        {package_depends, DepString},
        {package_shortdesc, Description ++ ", packaged for " ++ AppName ++ "."}, 
        {basedir, RelPath ++ "/" ++ DepPath},
        {parent_package, dep_to_packagename(AppName, DepNameList, ParentVersion)},
        {parent_version, "1"},
        {extra_templates, ExtraTemplates}
    ],

    pkgx_target_deb:run(Vars, OutputPath),
    make_dep_packages(BaseVars, AppName, Deps, SubDeps, ParentDeps, InstallPrefix, OutputPath);

make_dep_packages(_BaseVars, _AppName, [], _SubDeps, _ParentDeps, _InstallPrefix, _OutputPath) ->
    ok.

get_package_name(AppName, {DepName, DepVersion, _}) ->
    get_package_name(AppName, {DepName, DepVersion});
get_package_name(AppName, {DepName, DepVersion}) ->
    DepNameList = atom_to_list(DepName),
    CompatDepName = re:replace(DepNameList, "_", "-", [global, {return, list}]),
    AppName ++ "-" ++ CompatDepName ++ "-" ++ DepVersion.

compile_dep_list(AppName, [Dep|Deps], PackageNames) ->
    PackageName = get_package_name(AppName, Dep),
    compile_dep_list(AppName, Deps, [PackageName|PackageNames]);
compile_dep_list(_AppName, [], PackageNames) ->
    PackageNames.


make_release_package(BaseVars, AppName, Version, OldVersion, ErtsVsn, Deps, _ParentDeps, InstallLocation, OutputPath) ->
    InstallPrefix = InstallLocation ++  "/releases",
    RelPath = proplists:get_value(relpath, BaseVars, undefined),

    {ok, _} = file:copy(
        RelPath ++ "/bin/start_clean.boot",
        RelPath ++ "/releases/" ++ Version ++ "/start_clean.boot"),

    {ok, _} = file:copy(
        RelPath ++ "/releases/" ++ Version ++ "/" ++ AppName ++ ".boot",
        RelPath ++ "/releases/" ++ Version ++ "/start.boot"),

    file:copy(RelPath ++ "/releases/RELEASES", RelPath ++ "/releases/" ++ Version ++ "/RELEASES"),
    file:copy(RelPath ++ "/releases/start_erl.data", RelPath ++ "/releases/" ++ Version ++ "/start_erl.data"),

    ExtraTemplates = case OldVersion /= undefined of
        true ->
            [{"debian/preinst", deb_debian_preinst_dtl}];
        false ->
            []
    end,

    DepList     = compile_dep_list(AppName, Deps, []) ++ ["python", "python-apt"],
    DepString   = string:join(DepList, ", "),

    Vars = BaseVars ++ [
        {basedir, RelPath ++ "/releases/" ++ Version},
        {install_prefix, InstallPrefix}, 
        {install_dir_name, Version}, 
        {app, AppName}, 
        {version, "1"}, 
        {parent_version, "1"},
        {dep_version, Version},
        {erts_version, ErtsVsn},
        {package_name, AppName ++ "-release-" ++ Version}, 
        {parent_package, AppName ++ "-release-" ++ OldVersion},
        {package_depends, DepString},
        {package_shortdesc, "Release directory for " ++ AppName ++ " version " ++ Version}, 
        {extra_templates, [
            {"debian/prerm", deb_debian_meta_prerm_dtl},
            {AppName, bin_command_dtl, 8#755},
            {AppName ++ "_upgrade", upgrade_command_dtl, 8#755}
        ] ++ ExtraTemplates}
    ],

    pkgx_target_deb:run(Vars, OutputPath).


make_meta_package(BaseVars, AppName, Version, OldVersion, _Deps, _ParentDeps, InstallLocation, OutputPath) ->
    InstallPrefix = InstallLocation ++  "/releases",

    io:format("Oldversion: ~p~n", [OldVersion]),

    ExtraTemplates = case OldVersion /= undefined of
        true ->
            [{"debian/preinst", deb_debian_preinst_dtl}];
        false ->
            []
    end,

    OldDeps = case OldVersion /= undefined of
        true ->
            [AppName ++ "-release-" ++ OldVersion];
        false ->
            []
    end,

    DepList = OldDeps ++ [
        "python", 
        "python-apt", 
        AppName ++ "-release-" ++ Version
    ],

    DepString = string:join(DepList, ", "),
    RelPath = proplists:get_value(relpath, BaseVars, undefined),

    Vars = BaseVars ++ [
        {install_prefix, InstallPrefix}, 
        {install_dir_name, Version}, 
        {app_path, InstallLocation},
        {app, AppName}, 
        {package_name, AppName}, 
        {version, Version}, 
        {dep_version, Version}, %??
        {package_predepends, DepString},
        {package_shortdesc, "Meta install package and hot/cold upgrade scripts for " ++ AppName}, 
        {basedir, RelPath ++ "/releases/" ++ Version},
        {parent_package, AppName},
        {parent_version, OldVersion},
        {extra_templates, [
            {"debian/postinst", deb_debian_meta_upgrade_postinst_dtl},
            {AppName, proxy_bin_command_dtl, 8#755}
        ] ++ ExtraTemplates},
        {override_files, [
            {AppName, InstallPrefix ++ "/../bin"} % relocate main app command
        ]}
    ],

    pkgx_target_deb:run(Vars, OutputPath).

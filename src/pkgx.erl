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
     {user,     $U,     "user",     {string, "root"},       "User that will be running the app"},
     {output,   $o,     "output",   {string, "./packages"}, "Directory where the packages will be output"},
     {relpath,  $p,     "relpath",  {string, "./_build/$ver/rel/$relname"}, "The path to the releases dir"},
     {relname,  $n,     "relname",  string,                 "The release name you gave relx"},
     {buildver, $b,     "buildver", string,                 "The version to build"},
     {upfrom,   $u,     "upfrom",   string,                 "The version to upgrade from"},
     {confirm,  $c,     "confirm",  {boolean, true},        "Confirm build settings before proceeding"},
     {suffix,   $s,     "suffix",   {string, ""},           "String to append to all package names"}
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
    make_dep_packages(Options, AppName, ErtsDep, [], ParentDeps, InstallLocation, OutputPath, []),

    InstallPrefix = InstallLocation ++  "/lib",
    ExtraInstallFiles = make_dep_packages(Options, AppName, Deps, ErtsDep, ParentDeps, InstallPrefix, OutputPath, []),

    make_release_package(Options, AppName, ReleaseVsn, ParentReleaseVsn, ErtsVsn, Deps ++ ErtsDep, ParentDeps, InstallLocation, OutputPath),

    make_meta_package(Options, AppName, ReleaseVsn, ParentReleaseVsn, Deps ++ ErtsDep, ParentDeps, InstallLocation, OutputPath, ExtraInstallFiles).


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
        {ok,[{_,[{RelVersion,_,_}],_}]} ->
            %% if you get your relup gen in a mess, this can sometimes happen.
            %% this will prevent an infinite loop if a relup claims to upgrade from its own version.
            undefined;
        {ok,[{_,[{Previously,_,_}],_}]} -> 
            Previously;
        {error, _} ->
            undefined
    end,

    {ok, [{release, _, ErtsVersion, Deps}]} = file:consult(RelPath ++ "/releases/" ++ RelVersion ++ "/" ++ AppName ++ ".rel"),
    build_release_history(RelPath, AppName, PreviousVersion, [{RelVersion, PreviousVersion, ErtsVersion, Deps}|Releases]).


dep_to_packagename(AppName, Suffix, DepNameList, DepVersion) ->
    CompatDepName = re:replace(DepNameList, "_", "-", [global, {return, list}]),
    AppName ++ "-" ++ CompatDepName ++ "-" ++ DepVersion ++ Suffix.


make_dep_packages(BaseVars, AppName, [Dep|Deps], SubDeps, ParentDeps, InstallPrefix, OutputPath, InstallFiles) ->
    {DepName, DepVersion, DepPath} = Dep,
    DepNameList = atom_to_list(DepName),

    Suffix = proplists:get_value(suffix, BaseVars),
    PackageName = dep_to_packagename(AppName, Suffix, DepNameList, DepVersion),

    ParentVersion = proplists:get_value(DepName, ParentDeps, ""),
    ExtraTemplates = case DepVersion /= ParentVersion andalso ParentVersion /= "" of
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
    Basedir = RelPath ++ "/" ++ DepPath,

    ExtDependencies = case file:read_file(Basedir ++ "/priv/debian.dependencies") of
        {ok, DepContents} ->
            DepLines = binary:split(DepContents, <<"\n">>, [global]),
            [ binary_to_list(X) || X <- DepLines, X /= <<>> ];
        {error, _} ->
            []
    end,

    DepList     = compile_dep_list(AppName, Suffix, SubDeps, []),
    DepString   = string:join(DepList ++ ExtDependencies, ", "),

    Vars = BaseVars ++ [
        {install_prefix, InstallPrefix}, 
        {install_dir_name, DepNameList ++ "-" ++ DepVersion}, 
        {app, DepName}, 
        {package_name, PackageName}, 
        {version, "1"}, 
        {dep_version, DepVersion}, 
        {package_predepends, DepString},
        {package_shortdesc, Description ++ ", packaged for " ++ AppName ++ "."}, 
        {basedir, Basedir},
        {parent_package, dep_to_packagename(AppName, Suffix, DepNameList, ParentVersion)},
        {parent_version, "1"},
        {extra_templates, ExtraTemplates}
    ],

    ExtInstallFiles = case file:read_file(Basedir ++ "/priv/debian.install") of
        {ok, InstContents} ->
            Lines = binary:split(InstContents, <<"\n">>, [global]),
            lists:filtermap(
                fun(Line) -> 
                    case Line of
                        <<>> ->
                            false;
                        _ ->
                            [From, To] = binary:split(Line, <<"\t">>, [global]),
                            {true, [ DepPath ++ "/" ++ binary_to_list(From), binary_to_list(To) ]}
                    end
                end,
                Lines
            );
        {error, _} ->
            []
    end,

    pkgx_target_deb:run(Vars, OutputPath),
    make_dep_packages(BaseVars, AppName, Deps, SubDeps, ParentDeps, InstallPrefix, OutputPath, ExtInstallFiles ++ InstallFiles);

make_dep_packages(_BaseVars, _AppName, [], _SubDeps, _ParentDeps, _InstallPrefix, _OutputPath, InstallFiles) ->
    InstallFiles.

get_package_name(AppName, {DepName, DepVersion, _}, Suffix) ->
    get_package_name(AppName, {DepName, DepVersion}, Suffix);
get_package_name(AppName, {DepName, DepVersion}, Suffix) ->
    DepNameList = atom_to_list(DepName),
    CompatDepName = re:replace(DepNameList, "_", "-", [global, {return, list}]),
    AppName ++ "-" ++ CompatDepName ++ "-" ++ DepVersion ++ Suffix.

compile_dep_list(AppName, Suffix, [Dep|Deps], PackageNames) ->
    PackageName = get_package_name(AppName, Dep, Suffix),
    compile_dep_list(AppName, Suffix, Deps, [PackageName|PackageNames]);
compile_dep_list(_AppName, _Suffix, [], PackageNames) ->
    PackageNames.


make_release_package(BaseVars, AppName, Version, OldVersion, ErtsVsn, Deps, _ParentDeps, InstallLocation, OutputPath) ->
    InstallPrefix = InstallLocation ++  "/releases",
    RelPath = proplists:get_value(relpath, BaseVars, undefined),
    Suffix = proplists:get_value(suffix, BaseVars),

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

    OldPackage = case OldVersion /= undefined of
        true ->
            AppName ++ "-release-" ++ OldVersion ++ Suffix;
        false ->
            undefined
    end,

    DepList     = compile_dep_list(AppName, Suffix, Deps, []) ++ ["python", "python-apt"],
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
        {package_name, AppName ++ "-release-" ++ Version ++ Suffix}, 
        {parent_package, OldPackage},
        {package_depends, DepString},
        {package_shortdesc, "Release directory for " ++ AppName ++ " version " ++ Version}, 
        {extra_templates, [
            {"debian/postinst", deb_debian_meta_upgrade_postinst_dtl},
            {"debian/prerm", deb_debian_meta_prerm_dtl},
            {AppName, bin_command_dtl, 8#755},
            {AppName ++ "_upgrade", upgrade_command_dtl, 8#755}
        ] ++ ExtraTemplates}
    ],

    pkgx_target_deb:run(Vars, OutputPath).


make_meta_package(BaseVars, AppName, Version, OldVersion, _Deps, _ParentDeps, InstallLocation, OutputPath, ExtraInstallFiles) ->
    InstallPrefix = InstallLocation ++  "/releases",
    Suffix = proplists:get_value(suffix, BaseVars),

    io:format("Oldversion: ~p~n", [OldVersion]),

    ExtraTemplates = case OldVersion /= undefined of
        true ->
            [{"debian/preinst", deb_debian_preinst_dtl}];
        false ->
            []
    end,

    OldDeps = case OldVersion /= undefined of
        true ->
            [AppName ++ "-release-" ++ OldVersion ++ Suffix];
        false ->
            []
    end,

    DepList = OldDeps ++ [
        "python", 
        "python-apt", 
        AppName ++ "-release-" ++ Version ++ Suffix
    ],

    DepString = string:join(DepList, ", "),
    RelPath = proplists:get_value(relpath, BaseVars, undefined),

    Vars = BaseVars ++ [
        {install_prefix, InstallPrefix}, 
        {install_dir_name, Version}, 
        {app_path, InstallLocation},
        {app, AppName}, 
        {package_name, AppName ++ Suffix}, 
        {version, Version}, 
        {dep_version, Version}, %??
        {package_predepends, DepString},
        {package_shortdesc, "Meta install package for " ++ AppName}, 
        {basedir, RelPath},
        {parent_package, AppName ++ Suffix},
        {parent_version, OldVersion},
        {extra_templates, [
            {AppName, proxy_bin_command_dtl, 8#755}
        ] ++ ExtraTemplates},
        {override_files, [
            {AppName, InstallPrefix ++ "/../bin"} % relocate main app command
        ] ++ ExtraInstallFiles}
    ],

    pkgx_target_deb:run(Vars, OutputPath).

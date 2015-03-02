-module(pkgx).
-export([main/1]).

main(Targets) ->
    makepackages(Targets).

makepackages(_Targets) ->
    ok = application:load(erlydtl),
    ok = application:load(pkgx),
    
    ReleasesFile = "./releases/RELEASES",

    {ok, [ReleasesList]} = file:consult(ReleasesFile),
    [Release|_] = lists:sort(ReleasesList),
    {release, AppName, ReleaseVsn, ErtsVsn, Deps, _State} = Release,

    io:format(user, "Using release: ~s ~s~n", [AppName, ReleaseVsn]),

    Releases = build_release_history(AppName, ReleaseVsn),
    {ParentReleaseVsn, ParentDeps, _GrandparentDeps} = get_versions_to_replace(Releases),

    InstallPrefix = "/opt/" ++ AppName ++ "/lib",
    MetaInstallPrefix = "/opt/" ++ AppName ++ "/releases",
    OutputPath = "/home/vagrant/packages/",

    make_dep_packages(AppName, Deps, ParentDeps, InstallPrefix, OutputPath),

    ErtsDep = [{erts, ErtsVsn, "erts-" ++ ErtsVsn}],
    make_dep_packages(AppName, ErtsDep, ParentDeps, "/opt/" ++ AppName, OutputPath),

    make_release_package(AppName, ReleaseVsn, ParentReleaseVsn, ErtsVsn, Deps ++ ErtsDep, ParentDeps, MetaInstallPrefix, OutputPath),

    make_meta_package(AppName, ReleaseVsn, ParentReleaseVsn, Deps ++ ErtsDep, ParentDeps, MetaInstallPrefix, OutputPath).


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
    

build_release_history(AppName, RelVersion) ->
    build_release_history(AppName, RelVersion, []).

build_release_history(_AppName, undefined, Releases) ->
    lists:reverse(Releases);
build_release_history(AppName, RelVersion, Releases) ->
    PreviousVersion = case file:consult("./releases/" ++ RelVersion ++ "/relup") of
        {ok,[{_,[{Previously,_,_}],_}]} -> 
            Previously;
        {error, _} ->
            undefined
    end,

    {ok, [{release, _, ErtsVersion, Deps}]} = file:consult("./releases/" ++ RelVersion ++ "/" ++ AppName ++ ".rel"),
    build_release_history(AppName, PreviousVersion, [{RelVersion, PreviousVersion, ErtsVersion, Deps}|Releases]).


dep_to_packagename(AppName, DepNameList, DepVersion) ->
    CompatDepName = re:replace(DepNameList, "_", "-", [global, {return, list}]),
    AppName ++ "-" ++ CompatDepName ++ "-" ++ DepVersion.


make_dep_packages(AppName, [Dep|Deps], ParentDeps, InstallPrefix, OutputPath) ->
    {DepName, DepVersion, DepPath} = Dep,
    DepNameList = atom_to_list(DepName),
    PackageName = dep_to_packagename(AppName, DepNameList, DepVersion),

    ParentVersion = proplists:get_value(DepName, ParentDeps, undefined),
    ExtraTemplates = case DepVersion /= ParentVersion andalso ParentVersion /= undefined of
        true ->
            [{"debian/preinst", deb_debian_preinst_dtl}];
        false ->
            []
    end,

    Vars = [
        {install_prefix, InstallPrefix}, 
        {install_dir_name, DepNameList ++ "-" ++ DepVersion}, 
        {release_name, AppName}, 
        {app, DepName}, 
        {package_name, PackageName}, 
        {version, "1"}, 
        {dep_version, DepVersion}, 
        {package_author_name, "IRCCloud"}, 
        {package_author_email, "hello@irccloud.com"}, 
        {package_shortdesc, "A package"}, 
        {package_install_user, "vagrant"}, 
        {package_desc, "A longer package"}, 
        {basedir, DepPath},
        {parent_package, dep_to_packagename(AppName, DepNameList, ParentVersion)},
        {parent_version, "1"},
        {extra_templates, ExtraTemplates}
    ],

    pkgx_target_deb:run(Vars, OutputPath),
    make_dep_packages(AppName, Deps, ParentDeps, InstallPrefix, OutputPath);

make_dep_packages(_AppName, [], _ParentDeps, _InstallPrefix, _OutputPath) ->
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


make_release_package(AppName, Version, OldVersion, ErtsVsn, Deps, _ParentDeps, InstallPrefix, OutputPath) ->
    {ok, _} = file:copy(
        "bin/start_clean.boot",
        "releases/" ++ Version ++ "/start_clean.boot"),

    {ok, _} = file:copy(
        "releases/" ++ Version ++ "/" ++ AppName ++ ".boot",
        "releases/" ++ Version ++ "/start.boot"),

    file:copy("releases/RELEASES", "releases/" ++ Version ++ "/RELEASES"),

    ExtraTemplates = case OldVersion /= undefined of
        true ->
            [
                {"debian/preinst", deb_debian_preinst_dtl},
                {"debian/prerm", deb_debian_meta_prerm_dtl}
            ];
        false ->
            [
                {"debian/prerm", deb_debian_meta_prerm_dtl}
            ]
    end,

    DepList =   compile_dep_list(AppName, Deps, []) ++ ["python", "python-apt"],
    DepString = string:join(DepList, ", "),

    Vars = [
        {install_prefix, InstallPrefix}, 
        {install_dir_name, Version}, 
        {release_name, AppName}, 
        {app, AppName}, 
        {package_name, AppName ++ "-release-" ++ Version}, 
        {version, "1"}, 
        {dep_version, Version},
        {erts_version, ErtsVsn},
        {package_depends, DepString},
        {package_author_name, "IRCCloud"}, 
        {package_author_email, "hello@irccloud.com"}, 
        {package_shortdesc, "A package"}, 
        {package_install_user, "vagrant"}, 
        {package_desc, "A longer package"}, 
        {basedir, "releases/" ++ Version},
        {parent_package, AppName ++ "-release-" ++ OldVersion},
        {parent_version, "1"},
        {extra_templates, [
            {AppName, bin_command_dtl, 8#755}
        ] ++ ExtraTemplates}
    ],

    pkgx_target_deb:run(Vars, OutputPath).


make_meta_package(AppName, Version, OldVersion, _Deps, _ParentDeps, InstallPrefix, OutputPath) ->

    io:format("Oldversion: ~p~n", [OldVersion]),

    ExtraTemplates = case OldVersion /= undefined of
        true ->
            [
                {"debian/postinst", deb_debian_meta_upgrade_postinst_dtl},
                {"debian/preinst", deb_debian_preinst_dtl}
            ];
        false ->
            [{"debian/postinst", deb_debian_meta_postinst_dtl}]
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

    Vars = [
        {install_prefix, InstallPrefix}, 
        {install_dir_name, Version}, 
        {release_name, AppName}, 
        {app, AppName}, 
        {package_name, AppName}, 
        {version, Version}, 
        {dep_version, Version},
        {package_predepends, DepString},
        {package_author_name, "IRCCloud"}, 
        {package_author_email, "hello@irccloud.com"}, 
        {package_shortdesc, "A package"}, 
        {package_install_user, "vagrant"}, 
        {package_desc, "A longer package"}, 
        {basedir, "releases/" ++ Version},
        {parent_package, AppName},
        {parent_version, OldVersion},
        {extra_templates, [
            {AppName, bin_command_dtl, 8#755}, % main app command
            {AppName ++ "_upgrade", upgrade_command_dtl, 8#755} % upgrade command
        ] ++ ExtraTemplates},
        {override_files, [
            {AppName, InstallPrefix ++ "/../bin"}, % relocate main app command
            {AppName ++ "_upgrade", InstallPrefix ++ "/../bin"}, % relocate upgrade command
            {"../../bin/start_clean.boot", InstallPrefix ++ "/../bin"}
        ]}
    ],

    pkgx_target_deb:run(Vars, OutputPath).

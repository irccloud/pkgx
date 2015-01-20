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

    InstallPrefix = "/opt/" ++ AppName ++ "/lib",
    MetaInstallPrefix = "/opt/" ++ AppName ++ "/releases",
    OutputPath = "/home/vagrant/packages/",

    make_dep_packages(AppName, Deps, InstallPrefix, OutputPath),

    ErtsDep = [{erts, ErtsVsn, "erts-" ++ ErtsVsn}],
    make_dep_packages(AppName, ErtsDep, "/opt/" ++ AppName, OutputPath),
    make_meta_package(AppName, ReleaseVsn, Deps ++ ErtsDep, MetaInstallPrefix, OutputPath).

make_dep_packages(AppName, [Dep|Deps], InstallPrefix, OutputPath) ->
    io:format(user, "Package: ~p, Output: ~p~n", [Dep, OutputPath]),

    {DepName, DepVersion, DepPath} = Dep,

    DepNameList = atom_to_list(DepName),
    CompatDepName = re:replace(DepNameList, "_", "-", [global, {return, list}]),
    PackageName = AppName ++ "-" ++ CompatDepName ++ "-" ++ DepVersion,

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
        {basedir, DepPath}
        ],

    pkgx_target_deb:run(DepName, DepVersion, Vars, OutputPath),
    make_dep_packages(AppName, Deps, InstallPrefix, OutputPath);

make_dep_packages(_AppName, [], _InstallPrefix, _OutputPath) ->
    ok.

compile_dep_list(AppName, [Dep|Deps], PackageNames) ->
    {DepName, DepVersion, _} = Dep,
    DepNameList = atom_to_list(DepName),
    CompatDepName = re:replace(DepNameList, "_", "-", [global, {return, list}]),
    PackageName = AppName ++ "-" ++ CompatDepName ++ "-" ++ DepVersion,
    compile_dep_list(AppName, Deps, [PackageName|PackageNames]);

compile_dep_list(_AppName, [], PackageNames) ->
    PackageNames.


make_meta_package(AppName, Version, Deps, InstallPrefix, OutputPath) ->
    io:format(user, "Package: ~p, Output: ~p~n", [AppName, OutputPath]),

    {ok, _} = file:copy(
        "releases/" ++ Version ++ "/" ++ AppName ++ ".boot",
        "releases/" ++ Version ++ "/start.boot"),

    file:copy("releases/RELEASES", "releases/" ++ Version ++ "/RELEASES"),

    DepList = compile_dep_list(AppName, Deps, []),
    DepString = string:join(DepList, ", "),
    io:format(user, "Deps: ~p", [DepString]),

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
        {extra_templates, [
            {"debian/postinst", deb_debian_meta_postinst_dtl},
            {AppName, bin_command_dtl, 8#755}
        ]},
        {extra_files, [
            {AppName, InstallPrefix ++ "/../bin"},
            {"../../bin/start_clean.boot", InstallPrefix ++ "/../bin"}
        ]}
    ],

    pkgx_target_deb:run(AppName, Version, Vars, OutputPath).

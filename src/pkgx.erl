%% @author Arjan Scherpenisse <arjan@miraclethings.nl>
%% @copyright 2014 Arjan Scherpenisse
%% @doc Main

-module(pkgx).

-export([main/1]).

main(Targets) ->
    case length(Targets) of
        0 ->
            io:format(user, "usage: pkgx <deb|rpm>~n", []);
        _ ->
            makepackages(Targets)
    end.

makepackages(_Targets) ->
    ok = application:load(erlydtl),
    ok = application:load(pkgx),
    
    ReleasesFile = "./releases/RELEASES",

    {ok, [ReleasesList]} = file:consult(ReleasesFile),
    [Release|_] = lists:sort(ReleasesList),
    {release, AppName, ReleaseVsn, ErtsVsn, Deps, _State} = Release,

    io:format(user, "Using release: ~s ~s~n", [AppName, ReleaseVsn]),

    make_dep_packages(AppName, Deps, "/home/vagrant/packages/", ErtsVsn).

make_dep_packages(AppName, [Dep|Deps], Target, ErtsVsn) ->
    io:format(user, "Package: ~p, Output: ~p", [Dep, Target]),

    {DepName, DepVersion, DepPath} = Dep,

    DepNameList = atom_to_list(DepName),
    CompatDepName = re:replace(DepNameList, "_", "-", [global, {return, list}]),

    Vars = [
        %{replaces, GrandparentVersion},
        {release_name, AppName}, 
        {app, DepName}, 
        {orig_package_name, DepNameList}, 
        {package_name, AppName ++ "-" ++ CompatDepName ++ "-" ++ DepVersion}, 
        {version, "1"}, 
        {dep_version, DepVersion}, 
        {erts_version, ErtsVsn}, 
        {package_author_name, "IRCCloud"}, 
        {package_author_email, "hello@irccloud.com"}, 
        {package_shortdesc, "A package"}, 
        {package_install_user, "vagrant"}, 
        {package_desc, "A longer package"}, 
        {basedir, DepPath}
        ],

    pkgx_target_deb:run(DepName, DepVersion, Vars, Target),
    make_dep_packages(AppName, Deps, Target, ErtsVsn);

make_dep_packages(_AppName, [], _Target, _ErtsVsn) ->
    ok.


makemetapackage() ->
    ok = application:load(erlydtl),
    ok = application:load(pkgx),

    {ok, RelxTerms} = file:consult("./relx.config"),
    {default_release, RelName, _RelVer} = lists:keyfind(default_release, 1, RelxTerms),

    RelDir = lists:flatten(io_lib:format("_rel/~s", [RelName])),
    VarsFile = "pkgx.config",

    case filelib:is_regular(VarsFile) of
        false ->
            io:format(user, "File not found: ~s", [VarsFile]);

        true ->
            {ok, PkgVars} = file:consult(VarsFile),
            {package_name, _PkgName} = proplists:lookup(package_name, PkgVars),
            ReleasesFile = RelDir ++ "/releases/RELEASES",

            {ok, [ReleasesList0]} = file:consult(ReleasesFile),
            [Release|_] = lists:sort(ReleasesList0),
            {release, AppName, Vsn, ErtsVsn, _Deps, _Permanent} = Release,

            io:format(user, "Using release: ~s ~s~n", [AppName, Vsn]),

            Vars = [
                {user, AppName},
                {app, AppName}, 
                {version, Vsn}, 
                {erts_version, ErtsVsn}, 
                {basedir, RelDir}, 
                {relx, RelxTerms} 
                | PkgVars],

            build_metapackage(Vars),

            ok
    end.

build_metapackage(_Vars) -> 
    ok.

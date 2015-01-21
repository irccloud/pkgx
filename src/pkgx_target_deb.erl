-module(pkgx_target_deb).

-export([run/2]).

run(Vars, Target) ->
    PackageName = proplists:get_value(package_name, Vars),
    Version = proplists:get_value(version, Vars),

    DebFiles = filelib:wildcard(PackageName ++ "_" ++ Version ++ "-*.{deb,changes,build}", Target),
    case length(DebFiles) == 3 of
        true ->
            io:format(user, "Skipping ~p, version ~p is already packaged~n", [PackageName, Version]);
        false ->
            io:format(user, "Building ~p, version ~p. Output: ~p~n", [PackageName, Version, Target]),
            make_package(Vars, Target)
    end.

make_package(Vars, Target) ->
    Basedir = proplists:get_value(basedir, Vars),
    {ok, DirList} = file:list_dir(Basedir),

    InstallList = [ X || X <- DirList, X /= "debian" ],
    InstallPrefix = proplists:get_value(install_prefix, Vars),
    InstallDir = proplists:get_value(install_dir_name, Vars),

    Install = lists:map(
        fun(A) ->
            To = InstallPrefix ++ "/" ++ InstallDir,
            case filelib:is_file(Basedir ++ "/" ++ A) of
                true ->
                    {A, To};
                false ->
                    {A ++ "/", To}
            end
        end,
        InstallList
    ),

    ExtraFiles = proplists:get_value(extra_files, Vars, []),
    PkgVars = [ {install, Install ++ ExtraFiles} | Vars ],

    PkgName = proplists:get_value(package_name, PkgVars),
    Templates =
        [
         {"debian/changelog", deb_debian_changelog_dtl},
         {"debian/control", deb_debian_control_dtl},
         {"debian/rules", deb_debian_rules_dtl},
         {"debian/compat", <<"7">>},
         {"debian/" ++ PkgName ++ ".install", deb_debian_install_dtl}
        ],

    ExtraTemplates = proplists:get_value(extra_templates, PkgVars, []),
    TemplateMap = Templates ++ ExtraTemplates,

    lists:map(fun   ({F, V}) ->
                        TargetFile = filename:join(Basedir, F),
                        filelib:ensure_dir(TargetFile),
                        process_file_entry(TargetFile, V, PkgVars);
                    ({F, V, M}) when is_integer(M) ->
                        TargetFile = filename:join(Basedir, F),
                        filelib:ensure_dir(TargetFile),
                        process_file_entry(TargetFile, V, PkgVars),
                        file:change_mode(TargetFile, M)
              end,
              TemplateMap),

    Output = os:cmd("cd \"" ++ Basedir ++ "\" && debuild --no-tgz-check --no-lintian -i -us -uc -b"),
    io:format(user, "~s~n", [ unicode:characters_to_binary(Output) ]),

    Parent = filename:absname(filename:dirname(Basedir)),
    movefiles(filelib:wildcard(Parent ++ "/*.{deb,build,changes}"), Target),

    ok.

movefiles([FilePath|Files], To) ->
    FileName = lists:last(filename:split(FilePath)),
    file:rename(FilePath, To ++ FileName),
    movefiles(Files, To);
movefiles([], _To) ->
    ok.

process_file_entry(File, Module, Vars) when is_atom(Module) ->
    {ok, Output} = Module:render(Vars),
    process_file_entry(File, iolist_to_binary(Output), Vars);
process_file_entry(File, Output, _Vars) when is_binary(Output) ->
    ok = file:write_file(File, Output).

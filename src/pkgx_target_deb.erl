%% @author Arjan Scherpenisse <arjan@miraclethings.nl>
%% @copyright 2014 Arjan Scherpenisse
%% @doc Debian package generation for relx

-module(pkgx_target_deb).

-export([run/4]).

run(_AppName, _Vsn, Vars, Target) ->
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
         {"debian/compat", <<"7">>},
         {"debian/control", deb_debian_control_dtl},
         {"debian/copyright", deb_debian_copyright_dtl},
         {"debian/postrm", deb_debian_postrm_dtl},
         {"debian/rules", deb_debian_rules_dtl},
         {"debian/" ++ PkgName ++ ".install", deb_debian_install_dtl},
         {PkgName ++ ".config", package_config_dtl}
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

    Parent = filename:dirname(Basedir),
    movefiles(filelib:wildcard(Parent ++ "/*.deb"), Target),
    movefiles(filelib:wildcard(Parent ++ "/*.build"), Target),
    movefiles(filelib:wildcard(Parent ++ "/*.changes"), Target),

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

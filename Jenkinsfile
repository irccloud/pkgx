def erlang_version = "23.2.4"

def run_stage = { stageName, nodeName, work -> 
  node(nodeName) {
    stage (stageName) {
      try {
        work.call();
      } catch(error) {
        irccat("Jenkins %BOLD${env.JOB_NAME}%NORMAL (${env.BUILD_NUMBER}) %REDfailed%NORMAL %UNDERLINE${stageName}%NORMAL ${env.BUILD_URL}consoleFull %REVERSEerror:%NORMAL ${error}")
        throw error
      }
    }
  }
}

echo "erlang_version = ${erlang_version}"

node {
  run_stage('Checkout', 'ubuntu-20.04', {
    checkout scm
  })

  run_stage('Build', 'ubuntu-20.04', {
    withErlang(erlang_version) {
      shc("rebar3 escriptize")
    }
  }
}

def withErlang(String version, block) {
  def kerl_path = "/opt/erlang/${version}"
  withEnv(["PATH+KERL=${kerl_path}/bin",
           "PATH+KERL_INTERFACE=${kerl_path}/lib/erl_interface-*/bin",
           "REBAR_PLT_DIR=${kerl_path}"]) {
    sh("echo $PATH")
    sh("echo $REBAR_PLT_DIR")
    sh("rebar3 version")
    block()
  }
}

# ScriptShell

    It's a tool to run script. 
    

## usage

- Install Simple Build Tool

- Build

    sbt pack

- Command

    target\pack\bin\script-shell (options) [script] (name1=value1) (name2=value2)..

    - options
        -script [scriptfile] : run script file
        -noinput : not run shell

    - script
        -scala : scala script
        -javascript : java script

    - name=value
        set the value to the variable for the name.

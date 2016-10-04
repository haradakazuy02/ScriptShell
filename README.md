# ScriptShell

    This is a tool to run script. 

    It only run a step for each script line, so it cannot run into the calling method. 

    ex. script-shell -i javascript

    ex. script-shell -script test.scala -silent scala

## usage

- Install Simple Build Tool

- Build

    sbt pack

- Command

    target\pack\bin\script-shell (options) \[language] (name1=value1) (name2=value2)..

    - options
        -script \[scriptfile] : run script file
        -i : interpreter after running the [scriptfile]
        -init \[line] : run the script \[line] before start \[scriptfile]

    - language
        -scala : scala script
        -javascript : java script

    - name=value
        set the value to the variable for the name.

# remark

    For javascript, it recognize block end by bracket. So you should not input:

    for (i=0;i<10;i++)
    {
      print(i);
    }

    but should:

    for (i=0;i<10;i++) {
      print(i);
    }

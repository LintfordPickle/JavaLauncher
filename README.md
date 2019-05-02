# JavaLauncher (LDLibrary)
A Windows executable java launcher

# Usage
Place the Launcher.exe next to your .jar file. 
Then, create a configuration file (named either configuration.ini or <appname>.ini), and insert the following contents:

```
[Settings]
; Enter the path to the JRE (either relative from the launcher location or absolute).
jre=jre

; Enter the name of the application below (excluding .jar file extension)
app=Core

; Optionally add any jvm or application specific launch parameters
;params=

; Optionally add a web link to your application
;web=www.github.com/LintfordPickle/JavaLauncher
```
Afterwards, you can use the Launcher tool to configure and launch applications created with the LDLibrary project.

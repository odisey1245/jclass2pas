Command-line parameters:
jclass2pas [options] {package.class|package.}
options:
  -i  create include files: *h.inc - the list of classes/interfaces and array-types, *.inc - declarations
  -protected|public|private - what to be get:
    public - only public stuff,
    protected - public+protected
    private - all
  -classpath <path> - full file names .class or .jar and .zip, that are containing .class,
   it is allowed wildchars like * and ?, delimiter is semicolon (;)
  -o <name> - the name of output file(s) - FPC unit - only one unit is created,
   if not specified the one unit per package/class is created
  -x <package.class/package.>[=<name>] - consider specified class/package as already translated
   into unit <name> (will be included into uses-clause)

  -a <package.class/package.> - translate class or all classes of package anonymously
   (without contents) if possible

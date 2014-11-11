import syntaxtree.*;
import visitor.*;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;

public class J2V {
    public static void main(String[] Args) {
        File testDir = new File("../testers/hw3-tester/tests"); // create a folder called tests, and put the tests in it
        MiniJavaParser parse = null;
        for (final File fileEntry : testDir.listFiles()) {
            if (fileEntry.isFile()) {
                FileInputStream in = null;
                try {
                    in = new FileInputStream(fileEntry.getAbsoluteFile());
                }
                catch (FileNotFoundException err) {
                    err.printStackTrace();
                }

                try {
                    System.out.println("\n\nProcessing: " + fileEntry.getName());
                    if (null == parse) {
                        parse = new MiniJavaParser(in);
                    }
                    else {
                        parse.ReInit(in);
                    }
                    // put your logic here	
                    Node root = MiniJavaParser.Goal();
                    
                    SymbolTableConstructionVisitor visitor1 = new SymbolTableConstructionVisitor();
                    root.accept(visitor1);
                    SymbolTable st = visitor1.getSymbolTable();
                    J2VVisitor visitor2 = new J2VVisitor(st);
                    String result = st.getClassAndMethodDeclaration() + root.accept(visitor2, null);
                    System.out.println("---------------------------------\n" + result + "\n---------------------------------");
                }
                catch (ParseException e) {
                    System.out.println(e.toString());
                }
                catch (Error e1) {
                    System.out.println(e1.toString());
                }
            }
        }
    }
    
}
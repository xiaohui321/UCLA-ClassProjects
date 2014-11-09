

import syntaxtree.*;
import visitor.*;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;

public class J2V {
    public static void main(String[] Args) {
        File testDir = new File("../testers/hw2-tester-hui/tests"); // create a folder called tests, and put the tests in it
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
                    //System.out.println("Program parsed successfully");
                    
                    FirstRoundVisitor visitor1= new FirstRoundVisitor();
                    root.accept(visitor1);
                    SymbolTable st = visitor1.getsymbolTable();
                    GJDepthFirst<SymbolTable.Type,Object> visitor2 = new SecondRoundVisitor(st);
                    root.accept(visitor2,null);
                    
                    GJDepthFirst<String,Object> visitor3 = new ThirdRoundVisitor(st);
                    root.accept(visitor3,null);
                    System.out.println("Program type checked successfully");
                    
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
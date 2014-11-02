package MiniJava;

import syntaxtree.*;
import visitor.*;


import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;

public class TypeCheck {
    public static void main(String[] Args) {
        File testDir = new File("../tests/"); // create a folder called tests, and put the tests in it
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
                    System.out.println("Processing: " + fileEntry.getName());
                    if (null == parse) {
                        parse = new MiniJavaParser(in);
                    }
                    else {
                        parse.ReInit(in);
                    }
                    
                    // put your logic here	
                    Node root = MiniJavaParser.Goal();
                    System.out.println("Program parsed successfully");
                    root.accept(new DepthFirstVisitor());
                }
                catch (ParseException e) {
                    System.out.println(e.toString());
                }
            }
        }
    }
}
/*
import syntaxtree.*;
import visitor.*;

public class Typecheck {
	public static void main(String [] args) {
	   try {
           new MiniJavaParser(System.in);
           Node root = MiniJavaParser.Goal();
           System.out.println("Program parsed successfully");
           root.accept(new GJNoArguDepthFirst());
        }
        catch (ParseException e) {
           System.out.println(e.toString());
        }
   }
}




*/
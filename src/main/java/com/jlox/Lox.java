package com.jlox;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.nio.charset.Charset;
import java.nio.file.Files;
import java.nio.file.Paths;

public class Lox {
    public static boolean hadError;

    public static void main(String[] args) throws IOException {
        var len = args.length;
        if (len > 1) {
            System.out.println("Usage: jlox [path]");
        } else if (len == 1) {
            runFile(args[0]);
        } else {
            runPrompt();
        }
    }

    public static void runPrompt() throws IOException {
        InputStreamReader input = new InputStreamReader(System.in);
        BufferedReader reader = new BufferedReader(input);

        for (; ; ) {
            System.out.print("> ");
            var line = reader.readLine();
            // If line == null, user must have used CTRL+D
            if (line == null) break;
            run(line);
            hadError = false;
        }
    }

    private static void runFile(String path) throws IOException {
        byte[] bytes = new byte[0];
        try {
            bytes = Files.readAllBytes(Paths.get(path));
        } catch (java.nio.file.NoSuchFileException e) {
            System.err.println(e);
            System.exit(2);
        }
        run(new String(bytes, Charset.defaultCharset()));
        if (hadError) System.exit(1);
    }

    public static void run(String line) {
        System.out.println("Temp:" + line);
    }

    private static void report(int lineNumber, String where, String message) {
        System.err.println("[line " + lineNumber + "] Error" + where + ": " + message);
    }

    static void error(int lineNumber, String message) {
        report(lineNumber, "", message);
        hadError = true;
    }
}

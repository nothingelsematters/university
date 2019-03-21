package ru.ifmo.rain.naumov.walk;

import java.io.*;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.function.Predicate;
import java.util.stream.Stream;
import java.nio.file.InvalidPathException;

public class RecursiveWalk {
    public static void main(String[] args) {
        if (args == null || args.length != 2 || args[0] == null || args[1] == null) {
            System.out.println("Expected Argument Format: <input file name> <output file name>");
            return;
        }

        Path outputPath;
        try {
            outputPath = Path.of(args[1]);
            if (outputPath.getParent() != null) {
                Files.createDirectories(outputPath.getParent());
            }
        } catch (InvalidPathException | IOException e) {
            System.out.println("Can't make an output file");
            return;
        }

        try (FileWriter fileWriter = new FileWriter(outputPath.toString(), StandardCharsets.UTF_8)) {
            try (Stream<String> inputStream = Files.lines(Path.of(args[0]), StandardCharsets.UTF_8)) {

                inputStream.forEach(it -> {
                    try (Stream<Path> pathStream = Files.walk(Path.of(it))) {
                        pathStream.filter(Predicate.not(Files::isDirectory)).forEach(
                                that -> write(fileWriter, FNVHasher.getHash(that), that.toString()));

                    } catch (IOException | InvalidPathException e) {
                        writeError(fileWriter, it);
                    }
                });

            } catch (IOException e) {
                System.out.println("Reading Input File Error Occurred");

            } catch (UncheckedIOException | InvalidPathException e) {
                System.out.println("Invalid Input File");
            }

        } catch (IOException e) {
            System.out.println("Trying to Open an Output File Error Occurred");
        }
    }

    private static void write(Writer writer, int hash, String pathName) {
        try {
            writer.write(String.format("%08x %s%n", hash, pathName));
        } catch (IOException e) {
            System.out.println("Error writing into output file");
        }
    }

    private static void writeError(Writer writer, String pathName) {
        write(writer, 0, pathName);
    }
}

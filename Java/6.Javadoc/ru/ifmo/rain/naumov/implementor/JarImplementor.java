package ru.ifmo.rain.naumov.implementor;

import info.kgeorgiy.java.advanced.implementor.JarImpler;
import info.kgeorgiy.java.advanced.implementor.ImplerException;

import java.io.File;
import java.io.Writer;
import java.io.IOException;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.Files;
import java.nio.file.InvalidPathException;
import java.lang.reflect.Type;
import java.lang.reflect.ParameterizedType;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.lang.reflect.Parameter;
import java.lang.reflect.Executable;
import java.lang.reflect.Constructor;
import java.lang.reflect.TypeVariable;
import java.util.List;
import java.util.Arrays;
import java.util.HashSet;
import java.util.Comparator;
import java.util.zip.ZipEntry;
import java.util.stream.Collectors;
import java.util.function.Function;
import java.util.function.Supplier;
import java.util.function.Predicate;
import java.util.jar.Manifest;
import java.util.jar.Attributes;
import java.util.jar.JarOutputStream;
import javax.tools.ToolProvider;
import javax.tools.JavaCompiler;

/**
 * An implementation of {@link info.kgeorgiy.java.advanced.implementor.JarImpler}
 * — making a default implementation of a class or interface given with
 * {@link implement} or {@link implementJar}
 */
public class JarImplementor implements JarImpler {

    /**
     * A line separator constant string depending on Operation System
     */
    private static final String LINE_SEPARATOR = System.lineSeparator();

    /**
     * A space symbol string constant
     */
    private static final String SPACE = " ";

    /**
     * A tab constant (interpreted as 4 spaces)
     */
    private static final String TAB = SPACE.repeat(4);

    /**
     * An extension string for java source files
     */
    private static final String JAVA = "java";

    /**
     * An extension string for java compiled files
     */
    private static final String CLASS = "class";

    /**
     * Constructs an instance of a {@link JarImplementor} class
     */
    public JarImplementor() {}

    /**
     * Assert, whether any of the arguments passed is {@code null} or not
     *
     * @param  args            objects that are being examined
     * @throws ImplerException if any of the arguments given is {@code null}
     */
    private static void nullAssertion(Object... args) throws ImplerException {
        for (Object o: args) {
            if (o == null) {
                throw new ImplerException("Null arguments appeared");
            }
        }
    }

    /**
     * Creates the directories for the file path if they don't exist
     *
     * @param  path            file's path
     * @throws ImplerException if an error occurred trying to create folders
     */
    private static void createNeededDirectories(Path path) throws ImplerException {
        Path parent = path.getParent();
        if (parent == null) {
            return;
        }

        try {
            Files.createDirectories(parent);
        } catch (IOException e) {
            throw new ImplerException("Can't make an output file");
        }
    }

    /**
     * Returns a simple name of an implementation class
     *
     * @param  token a token of a class that is being implemented
     * @return       a simple name of an implementation class
     */
    private static String getSimpleImplName(Class<?> token) {
        return token.getSimpleName() + "Impl";
    }

    /**
     * Generates a path where an implementation class should be considering its package
     *
     * @param  root   a directory path where an implementation class package should be
     * @param  token  a token of a class that is being implemented
     * @param  suffix an extension of an implementation file
     * @return        a path where an implementation class should be
     */
    private static Path getPath(Path root, Class<?> token, String suffix) {
        return root.resolve(token.getPackageName().replace('.', File.separatorChar))
                    .resolve(getSimpleImplName(token) + "." + suffix);
    }

    /**
     * Returns a package of a class that is to be implemented
     *
     * @param  token a token of a class that is being implemented
     * @return       a package of a class that is to be implemented
     */
    private static String getPackage(Class<?> token) {
        return "package " + token.getPackageName() + ";";
    }

    /**
     * Generates a declaration for an implementation class.
     * It contains: modifiers, name, inheritance and needed generic parameters
     *
     * @param  token a token of a class that is being implemented
     * @return       a declaration for an implementation class
     */
    private static String getDeclaration(Class<?> token) {
        return String.format("public class %s%s %s %s%2$s",
                getSimpleImplName(token),
                getTemplateTypes(token.getTypeParameters()),
                token.isInterface() ? "implements" : "extends",
                token.getCanonicalName());
    }

    /**
     * Writes a header (package name and class declaration) using a writer given
     *
     * @param  token       a token of a class that is being implemented
     * @param  writer      a writer used to write a string generated
     * @throws IOException if an error occurred trying to write using a writer
     *
     * @see #getPackage(Class)
     * @see #getDeclaration(Class)
     */
    private static void writeHeader(Class<?> token, Writer writer) throws IOException {
        writer.write(getPackage(token) + LINE_SEPARATOR
                + getDeclaration(token));
    }

    /**
     * Joins with a coma an array with a given function applied to each argument
     *
     * @param  <T>        an array type
     * @param  array    an initial array
     * @param  function a function to apply to an array's elements  of type {@code T} turning it to a string
     * @return          a result of concatenation of the elements arisen from function application
     */
    private static <T> String joinWithComa(T[] array, Function<T, String> function) {
        return Arrays.stream(array).map(function).collect(Collectors.joining(", "));
    }

    /**
     * Returns a string representing template type arguments in triangular brackets
     *
     * @param  typeVariables an array of type variables which types to paste in brackets
     * @return               a string representing template type arguments in triangular brackets
     */
    private static String getTemplateTypes(TypeVariable<?>[] typeVariables) {
        return typeVariables.length == 0 ? "" :
            String.format("<%s>", joinWithComa(typeVariables, it -> it.getName().replace('$', '.')));
    }

    /**
     * Joins types given with a coma
     *
     * @param  types an array of types to join with a coma
     * @return       a string with types joined with a coma
     */
    private static String joinTypes(Type[] types) {
        return joinWithComa(types, Type::getTypeName);
    }

    /**
     * Writes a constructors of a class that is to be implemented
     *
     * @param  token           a token of a class being implemented
     * @param  writer          a writer to use
     * @throws ImplerException if there are no appropriate constructors in a class given
     * @throws IOException     if an error occurred trying to write with a given writer
     */
    private static void writeConstructors(Class<?> token, Writer writer)
            throws  ImplerException, IOException {
        List<Constructor<?>> constructors = Arrays.stream(token.getDeclaredConstructors())
                .filter(it -> !Modifier.isPrivate(it.getModifiers())).collect(Collectors.toList());

        if (constructors.size() == 0) {
            throw new ImplerException("No appropriate constructors find at all");
        }

        for (Constructor<?> constructor : constructors) {
            writer.write(getExecutable(constructor) + LINE_SEPARATOR);
        }
    }

    /**
     * Generates a return type and name of an executable object
     *
     * @param  executable executable object which return type and name are returned
     * @return            a string containing return type and name
     */
    private static String getReturnTypeAndName(Executable executable) {
        if (executable instanceof Method) {
            return String.join(SPACE, getTemplateTypes(executable.getTypeParameters()),
            ((Method) executable).getGenericReturnType().getTypeName().replace('$', '.'), executable.getName());
        }
        return getSimpleImplName(((Constructor<?>) executable).getDeclaringClass());
    }

    /**
     * Generates arguments and exceptions signatures of an executable object given
     *
     * @param  executable executable object which arguments and exceptions are returned
     * @return            a string containing arguments and exceptions
     */
    private static String getArgsAndExceptions(Executable executable) {
        Type[] exceptions = executable.getGenericExceptionTypes();
        StringBuilder stringBuilder = new StringBuilder();
        stringBuilder.append("(")
                .append(joinWithComa(executable.getParameters(), it -> it.toString().replace("$", ".")))
                .append(")")
                .append(exceptions.length == 0 ? "" :  " throws " + joinTypes(exceptions));
        return stringBuilder.toString();
    }

    /**
     * Generates a braced function body with correct indent
     *
     * @param  body a function body
     * @return      a braced function body
     *
     * @see #getBody(Executable)
     */
    private static String getFunctionBody(String body) {
        return " { " + LINE_SEPARATOR + TAB.repeat(2) + body + ";" + LINE_SEPARATOR + TAB + "}";
    }

    /**
     * Generates a method body.
     * Uses a default value as a return.
     *
     * @param  method a method which body is to be generated
     * @return        generated method body
     */
    private static String getMethodBody(Method method) {
        Class<?> returnValue = method.getReturnType();
        String string;
        if (returnValue.equals(void.class)) {
            string = "";
        } else if (returnValue.equals(boolean.class)) {
            string = "false";
        } else if (returnValue.isPrimitive()) {
            string = "0";
        } else {
            string = "null";
        }
        return getFunctionBody("return " + string);
    }

    /**
     * Generates a constructor body for a specified constructor
     * @param  constructor a constructor which body is returned
     * @return             a generated constructor body
     */
    private static String getConstructorBody(Constructor<?> constructor) {
        return getFunctionBody(Arrays.stream(constructor.getParameters()).map(Parameter::getName)
                .collect(Collectors.joining(", ", "super(", ")")));
    }

    /**
     * Generates a body of an executable object.
     * Returns a specialization for {@link Method}: {@link getMethodBody(Method)}
     * or a specialization for {@link Constructor}: {@link getConstructorBody(Constructor)}
     *
     * @param  executable an executable object which body is to be returned
     * @return            a generated body
     */
    private static String getBody(Executable executable) {
        if (executable instanceof Method) {
            return getMethodBody((Method) executable);
        }
        return getConstructorBody((Constructor<?>) executable);
    }

    /**
     * Generates a code for an executable object.
     * It containes the signature and the body.
     *
     * @param  executable an executable object to generate code of
     * @return            generated code
     *
     * @see #getReturnTypeAndName(Executable)
     * @see #getArgsAndExceptions(Executable)
     * @see #getBody(Executable)
     */
    private static String getExecutable(Executable executable) {
        StringBuilder stringBuilder = new StringBuilder();
        final int modifiers = executable.getModifiers() & ~Modifier.ABSTRACT
                        & ~Modifier.NATIVE & ~Modifier.TRANSIENT;

        stringBuilder.append(TAB)
                .append(Modifier.toString(modifiers) + SPACE)
                .append(getReturnTypeAndName(executable))
                .append(getArgsAndExceptions(executable))
                .append(getBody(executable))
                .append(LINE_SEPARATOR);

        return stringBuilder.toString();
    }

    /**
     * A helping function to return a set of methods of a class being implemeneted.
     * It applies a given function to a class token returning an array of methods,
     * which are filtered with a predicate and wrapped by a {@link MethodWrapper}
     *
     * @param  token             a class being implemented
     * @param  function          function applied to a token returning an array of methods
     * @param  predicate         a predicate by which a resulting array is filtered
     * @param  supplier          a supplier providing a hashset to collect the results
     * @return                   a hash set got performing described operations
     */
    private static HashSet<MethodWrapper> getMethodSet(Class<?> token,
                                        Function<Class<?>, Method[]> function,
                                        Predicate<Integer> predicate,
                                        Supplier<HashSet<MethodWrapper>> supplier) {

        return Arrays.stream(function.apply(token))
                .filter(it -> predicate.test(it.getModifiers()))
                .map(MethodWrapper::new)
                .collect(Collectors.toCollection(supplier));
    }

    /**
     * Collects a set of methods to implement of a class
     *
     * @param  token     a token of a class being implemented
     * @param  predicate which methods should be considered
     * @return           a hash set of methods to implement of a class being implemented
     */
    private static HashSet<MethodWrapper> getMethodSet(Class<?> token, Predicate<Integer> predicate) {
        HashSet<MethodWrapper> methods = getMethodSet(token, Class::getMethods, predicate, HashSet::new);

        while (token != null) {
            getMethodSet(token, Class::getDeclaredMethods, predicate, () -> methods);
            token = token.getSuperclass();
        }
        return methods;
    }

    /**
     * Writes generated methods of a class using a given writer
     *
     * @param  token       a token of a class being implemented
     * @param  writer      writer being used to write the results
     * @throws IOException if an error occurred trying to write using a given writer
     *
     * @see #getMethodSet(Class, Predicate)
     */
    private static void writeMethods(Class<?> token, Writer writer) throws IOException {
        for (MethodWrapper methodWrapper: getMethodSet(token, token.isInterface() ?
                Predicate.not(Modifier::isStatic) : Modifier::isAbstract)) {

            writer.write(getExecutable(methodWrapper.getMethod()) + LINE_SEPARATOR);
        }
    }

    /**
     * Writes a body of a class using a given writer.
     * Body mainly contains methods and constructors.
     *
     * @param  token           a token of a class being implemented
     * @param  writer          a writer being used to write the results
     * @throws ImplerException if there are no appropriate constructors in a class to implement
     * @throws IOException     if an error occurred trying to write using a given writer
     */
    private static void writeBody(Class<?> token, Writer writer) throws ImplerException, IOException {
        writer.write(" {" + LINE_SEPARATOR);
        if (!token.isInterface()) {
            writeConstructors(token, writer);
        }
        writeMethods(token, writer);
        writer.write("}");
    }

    /**
     * A private class that stores a {@link Method}.
     * Main feature differentiang it from a simple {@link Method}
     * is its {@link MethodWrapper#equals(Object)} and {@link MethodWrapper#hashCode()}
     * methods
     */
    private static class MethodWrapper {
        /**
         * A method being stored inside a wrapper
         */
        private final Method method;

        /**
         * Constructs a method wrapper by a given method
         *
         * @param method method to store
         */
        public MethodWrapper(Method method) {
            this.method = method;
        }

        /**
         * Gets a method stored
         *
         * @return a method stored
         */
        public Method getMethod() {
            return method;
        }

        /**
         * Overriden equals method.
         * Compares method wrappers by their methods' names, parameter types and return types
         *
         * @param  other a method wrapper to compare with
         * @return       whether a method wrapper given equals the stored one
         */
        @Override
        public boolean equals(Object other) {
            if (other == null) {
                return false;
            }

            if (other instanceof MethodWrapper) {
                MethodWrapper otherMethod = (MethodWrapper) other;
                return method.getName().equals(otherMethod.method.getName()) &&
                        Arrays.equals(method.getParameterTypes(), otherMethod.method.getParameterTypes()) &&
                        method.getReturnType().equals(otherMethod.method.getReturnType());
            }
            return false;
        }

        /**
         * {@inheritDoc}
         */
        @Override
        public int hashCode() {
            return method.getName().hashCode() ^ Arrays.hashCode(method.getParameterTypes())
                    ^ method.getReturnType().hashCode();
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void implement(Class<?> token, Path root) throws ImplerException {
        nullAssertion(token, root);
        if (token.isArray() || token.isPrimitive() || token == Enum.class
                || Modifier.isFinal(token.getModifiers())) {

            throw new ImplerException("Inappropriate class given");
        }

        root = getPath(root, token, JAVA);
        createNeededDirectories(root);

        try (Writer writer = Files.newBufferedWriter(root)) {
            writeHeader(token, writer);
            writeBody(token, writer);

        } catch (IOException e) {
            throw new ImplerException("Writing file error occurred", e);
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void implementJar(Class<?> token, Path jarFile) throws ImplerException {
        nullAssertion(token, jarFile);
        createNeededDirectories(jarFile);
        Path tmpdir;

        try {
            tmpdir = Files.createTempDirectory(jarFile.toAbsolutePath().getParent(), "tmp");
        } catch (IOException e) {
            throw new ImplerException("Failed to make a temporary directory", e);
        }

        try {
            implement(token, tmpdir);
            JavaCompiler javaCompiler = ToolProvider.getSystemJavaCompiler();
            String[] args = new String[] {
                "-cp",
                System.getProperty("java.class.path") + File.pathSeparator + tmpdir,
                tmpdir.resolve(getPath(tmpdir, token, JAVA)).toString()
            };

            if (javaCompiler == null || javaCompiler.run(null, null, null, args) != 0) {
                throw new ImplerException("Failed to compile generated java classes");
            }

            Manifest manifest = new Manifest();
            Attributes attributes = manifest.getMainAttributes();
            attributes.put(Attributes.Name.MANIFEST_VERSION, "1.0");

            try (JarOutputStream jarWriter = new JarOutputStream(Files.newOutputStream(jarFile), manifest)) {
                jarWriter.putNextEntry(new ZipEntry(token.getCanonicalName().replace('.', '/') + "Impl.class"));
                Files.copy(tmpdir.resolve(getPath(tmpdir, token, CLASS)), jarWriter);
            } catch (IOException e) {
                throw new ImplerException("Writing a jar file an error occurred", e);
            }

        } finally {
            try {
                Files.walk(tmpdir)
                    .map(Path::toFile)
                    .sorted(Comparator.reverseOrder())
                    .forEach(File::delete);
            } catch (IOException e) {
                throw new ImplerException("Failed deleting temporary files in " + tmpdir.toString());
            }
        }

    }

    /**
     * Implements a given class and stores it in a given path.
     * Usage: {@code <options> <class name> <path to store>}
     * Without any flags you get a java source file implementation.
     * And using {@code -jar} option you get a jar file implementation.
     *
     * @param  args        arguments of a described rules
     */
    public static void main(String[] args) {
        if (args == null || (args.length != 2 && args.length != 3)) {
            System.out.println("Wrong arguments format");
            return;
        }

        for (String arg : args) {
            if (arg == null) {
                System.out.println("Null argument appeared");
                return;
            }
        }
        JarImplementor jarImplementor = new JarImplementor();

        try {
            if (args.length == 2) {
                jarImplementor.implement(Class.forName(args[0]), Paths.get(args[1]));
            } else if (args.length == 3 && "-jar".equals(args[0])) {
                jarImplementor.implementJar(Class.forName(args[1]), Paths.get(args[2]));
            } else {
                System.out.println("Wrong arguments format");
            }

        } catch (ClassNotFoundException e) {
            System.out.println("Class not found");
        } catch (InvalidPathException e) {
            System.out.println("Invalid path given");
        } catch (ImplerException e) {
            System.out.println(e.getMessage());
        }
    }
}

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


public class JarImplementor implements JarImpler {

    private static final String LINE_SEPARATOR = System.lineSeparator();

    private static final String SPACE = " ";

    private static final String TAB = SPACE.repeat(4);

    private static final String JAVA = "java";

    private static final String CLASS = "class";

    public JarImplementor() {}

    private static void nullAssertion(Object... args) throws ImplerException {
        for (Object o: args) {
            if (o == null) {
                throw new ImplerException("Null arguments appeared");
            }
        }
    }

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

    private static String getSimpleImplName(Class<?> token) {
        return token.getSimpleName() + "Impl";
    }

    private static Path getPath(Path root, Class<?> token, String suffix) {
        return root.resolve(token.getPackageName().replace('.', File.separatorChar))
                    .resolve(getSimpleImplName(token) + "." + suffix);
    }

    private static String getPackage(Class<?> token) {
        return "package " + token.getPackageName() + ";";
    }

    private static String getDeclaration(Class<?> token) {
        return String.format("public class %s%s %s %s%2$s",
                getSimpleImplName(token),
                getTemplateTypes(token.getTypeParameters()),
                token.isInterface() ? "implements" : "extends",
                token.getCanonicalName());
    }

    private static void writeHeader(Class<?> token, Writer writer) throws IOException {
        writer.write(getPackage(token) + LINE_SEPARATOR
                + getDeclaration(token));
    }

    private static <T> String joinWithComa(T[] array, Function<T, String> function) {
        return Arrays.stream(array).map(function).collect(Collectors.joining(", "));
    }

    private static String getTemplateTypes(TypeVariable<?>[] typeVariables) {
        return typeVariables.length == 0 ? "" :
            String.format("<%s>", joinWithComa(typeVariables, it -> it.getName().replace('$', '.')));
    }

    private static String joinTypes(Type[] types) {
        return joinWithComa(types, Type::getTypeName);
    }

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

    private static String getReturnTypeAndName(Executable executable) {
        if (executable instanceof Method) {
            return String.join(SPACE, getTemplateTypes(executable.getTypeParameters()),
            ((Method) executable).getGenericReturnType().getTypeName().replace('$', '.'), executable.getName());
        }
        return getSimpleImplName(((Constructor<?>) executable).getDeclaringClass());
    }

    private static String getArgsAndExceptions(Executable executable) {
        Type[] exceptions = executable.getGenericExceptionTypes();
        StringBuilder stringBuilder = new StringBuilder();
        stringBuilder.append("(")
                .append(joinWithComa(executable.getParameters(), it -> it.toString().replace("$", ".")))
                .append(")")
                .append(exceptions.length == 0 ? "" :  " throws " + joinTypes(exceptions));
        return stringBuilder.toString();
    }

    private static String getFunctionBody(String body) {
        return " { " + LINE_SEPARATOR + TAB.repeat(2) + body + ";" + LINE_SEPARATOR + TAB + "}";
    }

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

    private static String getConstructorBody(Constructor<?> constructor) {
        return getFunctionBody(Arrays.stream(constructor.getParameters()).map(Parameter::getName)
                .collect(Collectors.joining(", ", "super(", ")")));
    }

    private static String getBody(Executable executable) {
        if (executable instanceof Method) {
            return getMethodBody((Method) executable);
        }
        return getConstructorBody((Constructor<?>) executable);
    }

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

    private static HashSet<MethodWrapper> getMethodSet(Class<?> token,
                                        Function<Class<?>, Method[]> function,
                                        Predicate<Integer> predicate,
                                        Supplier<HashSet<MethodWrapper>> supplier) {

        return Arrays.stream(function.apply(token))
                .filter(it -> predicate.test(it.getModifiers()))
                .map(MethodWrapper::new)
                .collect(Collectors.toCollection(supplier));
    }

    private static HashSet<MethodWrapper> getMethodSet(Class<?> token, Predicate<Integer> predicate) {
        HashSet<MethodWrapper> methods = getMethodSet(token, Class::getMethods, predicate, HashSet::new);

        while (token != null) {
            getMethodSet(token, Class::getDeclaredMethods, predicate, () -> methods);
            token = token.getSuperclass();
        }
        return methods;
    }

    private static void writeMethods(Class<?> token, Writer writer) throws IOException {
        for (MethodWrapper methodWrapper: getMethodSet(token, token.isInterface() ?
                Predicate.not(Modifier::isStatic) : Modifier::isAbstract)) {

            writer.write(getExecutable(methodWrapper.getMethod()) + LINE_SEPARATOR);
        }
    }

    private static void writeBody(Class<?> token, Writer writer) throws ImplerException, IOException {
        writer.write(" {" + LINE_SEPARATOR);
        if (!token.isInterface()) {
            writeConstructors(token, writer);
        }
        writeMethods(token, writer);
        writer.write("}");
    }

    private static class MethodWrapper {

        private final Method method;

        public MethodWrapper(Method method) {
            this.method = method;
        }

        public Method getMethod() {
            return method;
        }

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

        @Override
        public int hashCode() {
            return method.getName().hashCode() ^ Arrays.hashCode(method.getParameterTypes())
                    ^ method.getReturnType().hashCode();
        }
    }

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

## [Домашнее задание 1. Обход файлов](https://github.com/nothingelsematters/University/tree/master/Java/1.RecursiveWalk/)

+ Разработайте класс Walk, осуществляющий подсчет хеш-сумм файлов.

    + Формат запуска

       `java Walk <входной файл> <выходной файл>`

    + Входной файл содержит список файлов, которые требуется обойти.
    + Выходной файл должен содержать по одной строке для каждого файла. Формат строки:

        `<шестнадцатеричная хеш-сумма> <путь к файлу>`

    + Для подсчета хеш-суммы используйте алгоритм FNV.
    + Если при чтении файла возникают ошибки, укажите в качестве его хеш-суммы 00000000.
    + Кодировка входного и выходного файлов — UTF-8.
    + Если родительская директория выходного файла не существует, то соответствующий путь надо создать.
    + Размеры файлов могут превышать размер оперативной памяти.
    + Пример

        + Входной файл

            ```
            java/info/kgeorgiy/java/advanced/walk/samples/1
            java/info/kgeorgiy/java/advanced/walk/samples/12
            java/info/kgeorgiy/java/advanced/walk/samples/123
            java/info/kgeorgiy/java/advanced/walk/samples/1234
            java/info/kgeorgiy/java/advanced/walk/samples/1
            java/info/kgeorgiy/java/advanced/walk/samples/binary
            java/info/kgeorgiy/java/advanced/walk/samples/no-such-file```

        + Выходной файл

            ```
            050c5d2e java/info/kgeorgiy/java/advanced/walk/samples/1
            2076af58 java/info/kgeorgiy/java/advanced/walk/samples/12
            72d607bb java/info/kgeorgiy/java/advanced/walk/samples/123
            81ee2b55 java/info/kgeorgiy/java/advanced/walk/samples/1234
            050c5d2e java/info/kgeorgiy/java/advanced/walk/samples/1
            8e8881c5 java/info/kgeorgiy/java/advanced/walk/samples/binary
            00000000 java/info/kgeorgiy/java/advanced/walk/samples/no-such-file```


+ Усложненная версия:

    + Разработайте класс RecursiveWalk, осуществляющий подсчет хеш-сумм файлов в директориях

    + Входной файл содержит список файлов и директорий, которые требуется обойти. Обход директорий осуществляется рекурсивно.

    + Пример

        + Входной файл

        ```
        java/info/kgeorgiy/java/advanced/walk/samples/binary
        java/info/kgeorgiy/java/advanced/walk/samples```

        + Выходной файл

        ```
        8e8881c5 java/info/kgeorgiy/java/advanced/walk/samples/binary
        050c5d2e java/info/kgeorgiy/java/advanced/walk/samples/1
        2076af58 java/info/kgeorgiy/java/advanced/walk/samples/12
        72d607bb java/info/kgeorgiy/java/advanced/walk/samples/123
        81ee2b55 java/info/kgeorgiy/java/advanced/walk/samples/1234
        8e8881c5 java/info/kgeorgiy/java/advanced/walk/samples/binary```


+ При выполнении задания следует обратить внимание на:

    + Дизайн и обработку исключений, диагностику ошибок.
    + Программа должна корректно завершаться даже в случае ошибки.
    + Корректная работа с вводом-выводом.
    + Отсутствие утечки ресурсов.

+ Требования к оформлению задания.

    + Проверяется исходный код задания.
    + Весь код должен находиться в пакете ru.ifmo.rain.фамилия.walk.

---

## [Домашнее задание 2. Множество на массиве](https://github.com/nothingelsematters/University/tree/master/Java/2.ArraySet/)

+ Разработайте класс ArraySet, реализующие неизменяемое упорядоченное множество.
    + Класс ArraySet должен реализовывать интерфейс SortedSet (упрощенная версия) или NavigableSet (усложненная версия).
    + Все операции над множествами должны производиться с максимально возможной асимптотической эффективностью.

+ При выполнении задания следует обратить внимание на:
    + Применение стандартных коллекций.
    + Избавление от повторяющегося кода.

---

## [Домашнее задание 3. Студенты](https://github.com/nothingelsematters/University/tree/master/Java/3.StudentDB/)

+ Разработайте класс StudentDB, осуществляющий поиск по базе данных студентов.

+ Класс StudentDB должен реализовывать интерфейс StudentQuery (простая версия) или StudentGroupQuery (сложная версия).

+ Каждый метод должен состоять из ровно одного оператора. При этом длинные операторы надо разбивать на несколько строк.

+ При выполнении задания следует обратить внимание на:
    + Применение лямбда-выражений и потоков.
    + Избавление от повторяющегося кода.

---

## [Домашнее задание 4. Implementor](https://github.com/nothingelsematters/University/tree/master/Java/4.Implementor/)

+ Реализуйте класс Implementor, который будет генерировать реализации классов и интерфейсов.

+ Аргументы командной строки: полное имя класса/интерфейса, для которого требуется сгенерировать реализацию.

+ В результате работы должен быть сгенерирован java-код класса с суффиксом Impl, расширяющий (реализующий) указанный класс (интерфейс).

+ Сгенерированный класс должен компилироваться без ошибок.

+ Сгенерированный класс не должен быть абстрактным.

+ Методы сгенерированного класса должны игнорировать свои аргументы и возвращать значения по умолчанию.

+ В задании выделяются три уровня сложности:

    - Простой — Implementor должен уметь реализовывать только интерфейсы (но не классы). Поддержка generics не требуется.
    - Сложный — Implementor должен уметь реализовывать и классы и интерфейсы. Поддержка generics не требуется.
    - Бонусный — Implementor должен уметь реализовывать generic-классы и интерфейсы. Сгенерированный код должен иметь корректные параметры типов и не порождать UncheckedWarning.

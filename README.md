# ITMO University Computer Science course homeworks

## [Distributed Systems](distributed-systems)

- __Semester:__ 6

- __Course by:__ Roman Elizarov

- __Content:__ distributed systems homework in **Kotlin**

    <details>
        <summary>Course structure</summary>
        <ul>
            <li>
                <a href="distributed-systems/distributed-mutex">distributed mutex</a>
            </li>
        </ul>
    </details>

## [Functional Programming](functional-programming)

- __Semester:__ 6

- __Course by:__ Arseniy Seroka

- __Content:__ **haskell** homeworks

    <details>
        <summary>Course structure</summary>
        Practices:
        <ol>
            <li>
                <a href="functional-programming/practice1">Sorting functions</a>
            </li>
            <li>
                <a href="functional-programming/practice2">Number operations</a>
            </li>
        </ol>
        Homeworks:
        <ol start="0">
            <li>
                <a href="functional-programming/hw0">Theoretical</a>
            </li>
            <li>
                <a href="functional-programming/hw1">Basic language constructions</a>
            </li>
            <li>
                <a href="functional-programming/hw2">File System Shell</a>
            </li>
            <li>
                <a href="functional-programming/hw3">Strictness. Multithreading. Advanced types. Lenses. Comonads.</a>
            </li>
        </ol>
    </details>

## [Probability Theory](probability-theory)

- __Semester:__ 5-6

- __Course by:__ Irina Suslina

- __Content:__ probability algorithm implementations in **Octave**

    <details>
        <summary>Course structure</summary>
        <ul>
            <li>
                <a href="probability-theory/monte-carlo">Monte Carlo Method</a>
            </li>
        </ul>
    </details>

## [Parallel Programming](parallel-programming)

- __Semester:__ 5

- __Course by:__ Roman Elizarov, Nikita Koval

- __Content:__ parallelism homeworks in **Java** and **Kotlin**

    <details>
        <summary>Course structure</summary>
        <ul>
            <li>
                <a href="parallel-programming/stack-elimination">stack with elimination</a>
            </li>
            <li>
                <a href="parallel-programming/msqueue">Michael Scott queue</a>
            </li>
            <li>
                <a href="parallel-programming/fine-grained-bank">fine grained bank</a>
            </li>
            <li>
                <a href="parallel-programming/linked-list-set">linked list set</a>
            </li>
            <li>
                <a href="parallel-programming/dijkstra">parallel dijkstra</a>
            </li>
            <li>
                <a href="parallel-programming/monotonic-clock">monotonic clock</a>
            </li>
            <li>
                <a href="parallel-programming/universal-construction">universal construction</a>
            </li>
            <li>
                <a href="parallel-programming/mcs-lock">mcs lock</a>
            </li>
            <li>
                <a href="parallel-programming/faa-queue">fetch-and-add queue</a>
            </li>
            <li>
                <a href="parallel-programming/synchronous-queue">synchronous queue</a>
            </li>
            <li>
                <a href="parallel-programming/lock-free-bank">lock free bank</a>
            </li>
            <li>
                <a href="parallel-programming/stm-bank">stm bank</a>
            </li>
            <li>
                <a href="parallel-programming/blocking-stack">blocking stack</a>
            </li>
        </ul>
    </details>

## [Type Theory](type-theory)

- __Semester:__ 5

- __Course by:__ Dmitry Shtukenberg

- __Content:__ laboratory works in **Haskell** with make builds, alex and happy

    <details>
        <summary>Course structure</summary>
        <ul>
            <li>
                Lambda expression parser: <a href="type-theory/1-lambda-expression-parsing-haskell">haskell version</a>, <a href="type-theory/1-lambda-expression-parsing-ocaml">ocaml version</a>
            </li>
            <li>
                <a href="type-theory/2-normalization">normalization</a>
            </li>
            <li>
                <a href="type-theory/3-type-deduction">expression type reducing</a>
            </li>
        </ul>
    </details>

## [Cryptography](cryptography)

- __Semester:__ 5

- __Course by:__ Alla Levina

- __Content:__ symmetric, stream, assymmetric ciphers and cryptographic hash functions implementation mostly in **Kotlin**

    <details>
        <summary>Course structure</summary>
        <ul>
            <li><b>Symmetric Ciphers</b>
                <ul>
                    <li>
                        <a href="cryptography/kasiski-examination">Kasiski examination</a>: hacking Vigenere cipher
                    </li>
                    <li>
                        <a href="cryptography/des">des cipher</a> (Data Encryption Standard)
                    </li>
                    <li>
                        <a href="cryptography/serpent">serpent cipher</a>: Advanced Encryption Standard contest second place
                    </li>
                </ul>
            </li>
            <li><b>Stream Ciphers</b>
                <ul>
                    <li>
                        <a href="cryptography/rc4">rc4</a>: simpliest stream cipher
                    </li>
                    <li>
                        <a href="cryptography/comp128">a5 and comp128</a>: implementation of the A3, A5 and A8 functions defined in the GSM standard </br> A3 is used to authenticate the mobile station to the network. A8 is used to generate the session key used by A5 to encrypt the data transmitted between the mobile station and the BTS
                    </li>
                </ul>
            </li>
            <li><b>Assymmetric Ciphers</b>
                <ul>
                    <li>
                        <a href="cryptography/rsa">rsa</a>: simpliest modulo operation based assymmetric cipher, one of the first public-key cryptosystems and is widely used for secure data transmission
                    </li>
                </ul>
            </li>
            <li><b>Cryptographic Hash Functions</b>
                <ul>
                    <li>
                        <a href="cryptography/cubehash">CubeHash</a>: a cryptographic hash function submitted to the NIST hash function competition, SHA-3 semi-finalist
                    </li>
                </ul>
            </li>
    </details>

## [Translation Methods](translation-methods)

- __Semester:__ 5

- __Course by:__ Andrey Stankevich

- __Content:__ regex laboratory work in **Perl**, parsers in **Kotlin**

    <details>
        <summary>Course structure</summary>
        <ul>
            <li>
                <a href="translation-methods/regular-expressions">Regular Expressions in Perl</a>
            </li>
            <li>
                Manual building top-to-bottom syntax analyzers: <a href="translation-methods/c-function-header">c function headers</a>
            </li>
            <li>
                Using automatic analyzers generators Bison or ANTLR: <a href="translation-methods/functional2imperative">functional to imperative language translation</a>
            </li>
            <li>
                <a href="https://github.com/nothingelsematters/lalr-generator">LALR parser generator</a>
            </li>
        </ul>
    </details>

## [Operation Systems](operation-systems)

- __Semester:__ 4

- __Course by:__ Dmitry Banschikov

- __Content:__ homeworks in **c++** with make builds

    <details>
        <summary>Course structure</summary>
        <ul>
        <li>
            <a href="operation-systems/terminal">Interpreter</a>
        </li>
        <li>
            <a href="operation-systems/find">Find utility subset</a>
        </li>
        <li>
            <a href="operation-systems/pseudo-jit">Piece of JIT complier</a>
        </li>
        <li>
            <a href="operation-systems/libs-acquaintance">Introduction to libraries</a>
        </li>
        <li>
            <a href="operation-systems/synchronous-spcket-service">Introduction to sockets</a>
        </li>
        <li>
            <a href="operation-systems/net-descriptor-passing">Introduction to descriptors transferring and IPC</a>
        </li>
        <li>
            <a href="operation-systems/sigsegv-handler">Signals handling</a>
        </li>
        </ul>
    </details>

## [Mathematical Logic](mathematical-logic)

- __Semester:__ 4

- __Course by:__ Dmitry Shtukenberg

- __Content:__ laboratory works in **Haskell** with make builds, alex and happy

    <details>
        <summary>Course structure</summary>
        <ul>
            <li>
                <a href="mathematical-logic/1-expression-parser">expression parser</a>
            </li>
            <li>
                <a href="mathematical-logic/2-proof-minimization">proof minimization</a>
            </li>
            <li>
                <a href="mathematical-logic/3-intuitionistic-proof-conversion">intuitionistic proof conversion</a>
            </li>
            <li>
                <a href="mathematical-logic/4-propositional-calculus-completeness">propositional calculus completeness</a>
            </li>
            <li>
                <a href="mathematical-logic/5-formal-arithmetic-proof-check">formal arithmetic proof check</a>
            </li>
        </ul>
    </details>

## [Java](java)

- __Semester:__ 3

- __Course by:__ Georgiy Korneev

- __Content:__ homeworks in **Java** with buildscripts and javadoc

    <details>
        <summary>Course structure</summary>
        <ul>
            <li>
                <a href="java/1-recursive-walk">recursive walk</a>
            </li>
            <li>
                <a href="java/2-array-set">array set</a>
            </li>
            <li>
                <a href="java/3-student-db">student db</a>
            </li>
            <li>
                <a href="java/4-implementor">implementor</a>
            </li>
            <li>
                <a href="java/5-jarimplementor">jar implementor</a>
            </li>
            <li>
                <a href="java/6-javadoc">javadoc</a>
            </li>
            <li>
                <a href="java/7-iterative-parallelism">iterative parallelism</a>
            </li>
            <li>
                <a href="java/8-parallel-mapper">parallel mapper</a>
            </li>
            <li>
                <a href="java/9-web-crawler">web crawler</a>
            </li>
            <li>
                <a href="java/10-hello-udp">hello udp</a>
            </li>
            <li>
                <a href="java/11-private-persons">private persons</a>
            </li>
        </ul>
    </details>

## [Web Programming](web)

- __Semester:__ 3

- __Course by:__ Mike Mirzayanov

- __Content:__ codeforces site implementation homeworks in **Java** with **Tomcat** ans **Spring**

    <details>
        <summary>Course structure</summary>
        <ul>
        <li>
            <a href="web/1-server">HTTP (cURL usage, HTTP-requests, simple HTTP server)</a>
        </li>
        <li>
            <a href="web/2-front">Верстка (HTML + CSS)</a>
        </li>
        <li>
            <a href="web/3-servlets">Servlet API (Tomcat, JSON, CaptchaFilter)</a>
        </li>
        <li>
            <a href="web/4-login">Servlet API 2 (Java reflection, file database, Freemaker)</a>
        </li>
        <li>
            <a href="web/5-webmail">SQL (SQL basics, refactoring with Java reflection, MariaDB)</a>
        </li>
        <li>
            <a href="web/6-js">AJAX (Javascript, AJAX)</a>
        </li>
        <li>
            <a href="web/7-spring">Spring (Spring Boot)</a>
        </li>
        <li>
            <a href="web/8-table-relations">Spring (OneToMany, ManyToOne, ManyToMany relations)</a>
        </li>
        <li>
            <a href="web/9-vue-js">Vue.js (Basics)</a>
        </li>
        <li>
            <a href="web/10-spring-api">Spring Rest API w/ Vue.js frontend</a>
        </li>
        </ul>
    </details>

## [C++](c++)

- __Semesters:__ 2-3

- __Course by:__ Ivan Sorokin

- __Content:__ **c++** homework projects

    <details>
        <summary>Course structure </summary>
        <ul>
        <li>
        <a href="https://github.com/nothingelsematters/similar-files">Similar Files Finder</a>: An utility to find files with similar content in directories
        <li>
        <a href="https://github.com/nothingelsematters/substring-finder">Substring Finder</a>: An utility to find the given substring in directories
        <li>
        <a href="https://github.com/nothingelsematters/function">Function</a>: <code>std::function</code> implementation
        </ul>
    </details>

## [Descrete Mathematics](discrete-maths)

- __Semesters:__ 1-4

- __Course by:__ Andrey Stankevich, Artem Vasilyev

- __Content:__ laboratory works mostly in **c++**

    <details>
        <summary>Course structure</summary>
        <ul>
        <li>
            <a href="descrete-maths/probability">Probability</a>
        </li>
        <li>
            <a href="descrete-maths/languages">Language Theory and Automats</a>
        </li>
        <li>Context Free Grammars</li>
        <li>
            <a href="descrete-maths/hamilton-path">Hamilton Paths</a>
        </li>
        <li>
            <a href="descrete-maths/graph-planarity">Graph Planarity</a>
        </li>
        <li>
            <a href="descrete-maths/generating-function">Generating Function</a>
        </li>
        <li>
            <a href="descrete-maths/turing-machine">Turing machine</a>
        </li>
        </ul>
    </details>

## [Algorithms and Data Structures](algorithms-and-data-structures)

- __Semesters:__ 1-4

- __Course by:__ Pavel Mavrin

- __Content:__ laboratory works mostly in **c++**

    <details>
        <summary>Course structure</summary>
        <ul>
        <li>
            <a href="algorithms-and-data-structures/dynamic-programming">Dynamic Programming</a>
        </li>
        <li>
            <a href="algorithms-and-data-structures/egment-tree">Segment Tree</a>
        </li>
        <li>
            <a href="algorithms-and-data-structures/binary-search-tree">Binary Search Tree</a>
        </li>
        <li>
            <a href="algorithms-and-data-structures/tree-algorithms">Tree Algorithms: LCA, Link-Cut, etc</a>
        </li>
        <li>
            <a href="algorithms-and-data-structures/greed">Greed Algorithms</a>
        </li>
        <li>
            <a href="algorithms-and-data-structures/graphs">Graphs</a>
        </li>
        <li>
            <a href="algorithms-and-data-structures/minimum-path">Minimum Paths</a>
        </li>
        <li>
            <a href="algorithms-and-data-structures/strings">Strings</a>
        </li>
        <li>
            <a href="algorithms-and-data-structures/flow-and-matching">Maximum Flow And Matching</a>
        </li>
        <li>
            <a href="algorithms-and-data-structures/maximum-flow-minimum-cost">Maximum Flow Minimum Cost</a>
        </li>
        <li>
            <a href="algorithms-and-data-structures/mathematic">Mathematic</a>
        </li>
        </ul>
    </details>

## [Paradigms of Programming](paradigms-of-programming)

- __Semester:__ 2

- __Course by:__ Georgiy Korneev, Nikolay Vedernikov

- __Content:__ homeworks in **Java** with some contracts and single **JavaScript** parser

    <details>
        <summary>Course structure</summary>
        <ul>
            <li>
                <a href="paradigms-of-programming/1-calc-sah256">calc sha256</a>
            </li>
            <li>
                <a href="paradigms-of-programming/2-binary-search">binary search</a>
            </li>
            <li>
                <a href="paradigms-of-programming/3-array-queue">array queue</a>
            </li>
            <li>
                <a href="paradigms-of-programming/4-queue">queue</a>
            </li>
            <li>
                <a href="paradigms-of-programming/5-evaluate">evaluate</a>
            </li>
            <li>
                <a href="paradigms-of-programming/6-expression-parser">expression parser</a>
            </li>
            <li>
                <a href="paradigms-of-programming/7-exceptions">exceptions</a>
            </li>
            <li>
                <a href="paradigms-of-programming/9-functional-expression">functional expression</a>
            </li>
            <li>
                <a href="paradigms-of-programming/10-object-expression">object expression</a>
            </li>
        </ul>
    </details>

---

## [Extra Machine Learning course](machine-learning)

- __Course by:__ Huawei workers

- __Content:__ different maching learning algorithm implementation

    <details>
        <summary>Course structure</summary>
        <ul>
            <li>
                <a href="machine-learning/kNN">kNN classifying method</a>
            </li>
            <li>
                <a href="machine-learning/linear-regression">linear regression</a>: using stochastic gradient descent with momentum and matrix method
            </li>
            <li>
                <a href="machine-learning/small-tasks">different tasks</a>
            </li>
        </ul>
    </details>

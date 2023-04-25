# Simple SBT project for Scala 3

## Running the main methods

There are 3 _main methods_ available in the `src/scala/Main.scala` file. To use them from the SBT command prompt issue one of the following command sequences:

To execute the `main_1` method:

```
runMain main_1
```

To invoke the `main_2` method with an `Int` argument `123`:

```
runMain main_2 123
```

To execute the `main_3` method with arguments:

```
runMain main_3 Bob 12 false aaa bbb ccc
```

or

```
runMain main_3 Alice 11 true aaa bbb ccc
```


## Scalafmt

The project also contains a configuration for the [`Scalafmt`](https://scalameta.org/scalafmt/) tool, which can be used to automatically format the source code.

To check wheather the code requires any adjustments, from the SBT commandline, issue the following command:

```
scalafmtCheck
```

To actually apply the needed changes invoke:

```
scalafmt
```



# 🐝 bee

Bee is a simple programming language I created as a personal learning project. It was born out of my desire to delve deeper into the world of compilers and interpreters. I wanted to understand how code transforms from human-readable instructions into something a computer can understand.

## Language Features

**Data Types:**

* **String:**  A sequence of characters enclosed in double quotes (").
* **Char:**  A single character enclosed in single quotes (').
* **Integer:**  Whole numbers without decimal points.
* **Float:**  Numbers with decimal points.
* **Boolean:**  True or False values.
* **Nil:** Null value.

**Syntax:**

* **Punctuators:**
  * `()` Parentheses for function calls and expressions.
  * `.` Dot for accessing members of objects.
  * `;` Semicolon to terminate statements.
* **Operators:**
  * `+` Addition
  * `-` Subtraction
  * `/` Division
  * `*` Multiplication
  * `>` Greater than
  * `<` Less than
  * `>=` Greater than or equal to
  * `<=` Less than or equal to
  * `!=` Not equal to
  * `==` Equal to
  * `=` Assignment
  * `!` Logical negation
* **Identifiers:**
  * A sequence of letters, digits, and underscores, starting with a letter or underscore.
* **Keywords:**
  * `mut`  Declares a mutable variable.
  * `echo`  Outputs a value to the console.
  * `nil` Nill value.
  * `true | false` Booleans.
* **Comments:**
  * Single-line comments start with `//`.

**Statements:**

* **Echo Statement:**  Outputs a value to the console.

    ```rust
    echo "Hello, World";
    echo 10 * 5;
    ```

* **Expression Statement:**  Evaluates an expression.

    ```rust
    10 * 5 --5;
    variable = 10;
    !true == false;
    ```

* **Variable Declaration:**  Declares a variable.

    ```rust
    mut? name = "drezzy"; // constant
    mut age = 18;
    ```

### Interpreter

Bee comes with a simple interpreter called `cbee`. You can install it using npm:

```bash
npm i cbee --g
```

**Running Bee Code:**

* **REPL:**  Start the interactive REPL:

    ```bash
    cbee
    ```

* **Running a file:**  Run a Bee file:

    ```bash
    cbee [filename]
    ```

* **Help:**  Display help information:

    ```bash
    cbee --help
    ```

### Example Program

```rust
// This is a comment
mut name = "Bee";
mut age = 1;

echo "Hello, " + name + "!";
echo "You are " + age + " year(s) old.";
```

### Contributing

Contributions are welcome! Please feel free to submit issues, pull requests, and suggestions on the GitHub repository.

### License

Bee is licensed under the MIT license. See LICENSE for more details.

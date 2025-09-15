### 关于 OLang

OLang 是我正在开发的一款实验性的通用编程语言，主要用来测试我对编程语言语法的各种构想，顺便探索下类型系统、生命周期管理、模板元编程等相关内容。目前 OLang 的大部分语法结构已经基本确定，但在不可变性表达、泛型参数约束、模块导入导出等方面仍在推敲和修改当中，未来可能会有较大变化。OLang 当前主要使用 Python 3.13 开发，在安装好 Python 后即可运行项目中的 [`main.py`](/main.py) 脚本来编译和运行 [`test.olang`](/test.olang) 中的 OLang 测试代码。OLang 的语法规范详见 [`lang_spec.py`](/lang_spec.py)；解析 OLang 源码所使用的 LR(1) Parser 实现代码参见 [`parser.py`](/parser.py)。

### 语法介绍

OLang 的语法设计主要借鉴了 [Zig](https://ziglang.org/)，[Rust](https://www.rust-lang.org/)，[Haskell](https://www.haskell.org/) 这几门语言，其目标是在维持语法一致性和简洁性的同时尽可能地提高语言的表达能力与可扩展性。

#### 变量

OLang 中的变量使用 `let` 关键字声明，可以在变量名后使用冒号注解其能够接受的类型。变量声明目前暂不支持指定不可变性，关于这个方面计划在未来斟酌后添加相应的语法，目前主要考虑使用 Rust 中 `mut` 关键字或是 Zig 中 `const` 关键字的形式。下面是一些符合 OLang 语法的变量声明示例：

```
let v1: bool;  // 声明一个 v1 变量并标注其类型为 bool
let v2 = 100;  // 声明一个 v2 变量并初始化为 100
let v3, v4: i32, v5 = "hello", 14, TRUE;  // 可以一次性声明多个变量
```

#### 字面量类型

OLang 目前内置的字面量类型包括 `i32`，`bool` 和 `str`，这些字面量类型的默认都是不可变的，可以用下面的语法创建对应类型的值：

```
let v1 = 2345;  // 整数字面量
let v2, v3 = TRUE, FALSE;  // 布尔字面量
let v4 = "Hello, world!";  // 字符串字面量
```

#### 结构化类型

OLang 为元组和结构体这两种编程中常用的结构化数据类型提供了原生语法支持，要在 OLang 中创建元组可以使用如下语法：

```
let tup1 = ();  // 创建一个空元组
let tup2 = 5,;  // 如果元组只有一个元素，则末尾的逗号不能省略
let tup3 = 1, 3, 5;  // 多个元素的元组，末尾的逗号可有可无
let tup3 = (2, 4, 6);  // 当然如果你想的话也可以在元组两侧添加括号
```

OLang 的结构体语法相比其它语言稍有不同，字段声明使用的是 `let` 表达式：

```
let stct = .(
    let fld1 = 5;
    let fld2 = "Hello, world!";
    let fld3 = TRUE  // 最后一个字段后可以省略分号
);
```

不过 OLang 对使用 `.( )` 创建结构体的语法提供了一些特殊照顾，声明字段时的 `let` 关键字可以省略，末尾的分号也可以换成逗号，所以写成下面这个样子也是可以的：

```
// 这种写法更像其它语言中创建结构体的语法
let stct = .(fld1 = TRUE, fld2 = "Hello, world!", fld3 = 5);

// 支持混搭语法，但并不推荐
let mixed = .(
    let fld1 = 5,
    fld2 = "Hello, world!";
    fld3 = TRUE,
);
```

除了上述语法之外 OLang 中还可以使用 `.{ }` 创建代码块结构体，它和后文会介绍的[代码块](#代码块)很像，两者的主要区别在于：

1. 代码块结构体中的 `let` 表达式对应的是字段声明；
2. 代码块结构体的求值结果是一个包含了这些声明字段的结构体。

下面是一个具体的示例，注意代码块结构体没有语法特殊照顾，所以不能省略 let 或是替换分号：

```
let stct = .{
    let fld1 = 5;
    print(fld1 + 7);  // 除了字段声明外还可以插入别的语句
    let fld2 = "Hello, world!";
    let fld3 = TRUE
};
```

编程中也常常会用到具有类型名称的结构化数据类型，为此 OLang 提供了 `tuple` 和 `struct` 关键字来声明具名元组和具名结构体类型：

```
// 创建一个名为 Point3D 的具名元组类型
tuple Point3D (i32, i32, i32)  // 声明结尾的分号可以省略
let val1 = Point3D(3, 5, 7);

// 创建一个名为 Point2D 的具名结构体类型
struct Point2D .(
    let x = i32;
    let y = i32
)
let val2 = Point2D.(x = 3, y = 5);
```

#### 解构赋值

OLang 也支持 JS、Python、Rust 等语言中常见的解构赋值，利用它可以便捷地展开和使用结构化的数据。OLang 的解构赋值语法遵循一条简单的规则，只要将创建结构化数据时填值的位置替换成解构的变量名即可，下面是一些 OLang 中解构赋值的代码示例：

```
// 解构元组，得到变量 a = 5, b = "Hello", c = TRUE
let a, b: str, c = (5, "Hello", TRUE);

// 解构结构体，得到变量 d = 5, e = 7
let .(let foo = d, let bar = e) = .{
    let foo = 5;
    let bar = 7
};

// 重新赋值时使用解构
a, b, c = 13, "World", FALSE;

// 甚至可以混搭，但并不推荐
(let f), a, (let g) = "Yes", 7, "No";
```

请注意在 OLang 中逗号运算符的结合优先级比等号运算符更高，这种设计是 OLang 语法上仔细斟酌取舍后的结果，如果反转等号与逗号的优先级，那解构赋值就必须写成下面这样：

```
(a, b, c) = (1, 3, 5);  // 省略两边的括号会让整条语句变成一个元组表达式
```

这种设计不好的地方在于会让下面这种代码变得很有迷惑性：

```
a = 3, b = 5, c = 7;  // 这行代码做了什么？
```

这行代码看起来像是一连串的赋值，但根据前文提到的优先级关系，它实际上等价于下面这种写法：

```
a = (3, b) = (5, c) = 7;
```

所以如果需要在一行进行多个变量的赋值，建议优先使用解构赋值语法；如果非要使用前面的写法，请在合适的位置添加括号：

```
a, b, c = 3, 5, 7;  // 直接使用解构赋值
(a = 3), (b = 5), (c = 7);  // 使用括号改变结合顺序
```

#### 代码块

OLang 中可以使用一对大括号建立代码块，块内的多条语句会按照顺序执行，块中声明的变量仅能在块内引用。对代码块的求值结果是其中最后一条被执行语句的值，空代码块的求值结果为空元组：

```
let result = {
    let a = 3;
    let b = 5;
    a + b;  // 这里的结果会被赋给 result
};
let empty = {};  // 等同于 let empty = ();
```

请注意，下面这种写可能让熟悉 C 或 Python 的开发者感到迷惑：

```
let tup = { 1, 3, 5 };
```

等号右边看起来很像 C 中的数组初始化或是 Python 中的集合创建，但它其实是一个只包含一条语句的代码块，这条语句创建了一个元组，其后的分号被省略了。

#### 函数

OLang 中的函数可以通过 `fn` 关键字定义：

```
// 定义一个名为 foo 的函数
fn foo(x: i32, y: i32) {
    x + y
}

// 调用定义好的 foo 函数
let result = foo(3, 5);
```

需要注意的是 OLang 中的函数只能有一个参数和返回值，所以上述示例中的 `foo` 函数其实是一个接收 `(i32, i32)` 类型参数的函数。为了更清晰地展示这其中的区别，我们可以将它改成如下等价形式：

```
fn foo (pair: (i32, i32)) {
    let x, y = pair;
    x + y
}
```

OLang 函数的形参声明实际上相当于前文介绍过的[解构赋值](#解构赋值)表达式的左侧，它会将函数调用时接收到的实参解构到当前的上下文中供函数体执行使用。对应的，`foo(3, 5)` 这种写法其实是先创建了元组 `(3, 5)`，然后再将其传递给 `foo` 函数执行调用。

需要注意的是，OLang 中的括号基本只用作改变代码语法元素的结合顺序，函数调用在没有歧义的情况下可以省略实参两侧的括号：

```
let args = 3, 5;
let result = foo args;  // 调用 foo 函数
```

特别地，因为函数调用的结合优先级比逗号运算符高，所以 `foo 3, 5` 这种写法实际上等同于 `(foo(3), 5)`，其结果是一个元组。

另外请注意，因为 OLang 函数语法的特殊设计，下面这两个函数所接受的参数类型并不等价：

```
fn foo(val: i32) { val }  // 该函数的参数类型为 i32
fn bar(val: i32,) { val }  // 该函数的参数类型为 (i32,)
```

#### 匿名元素

在 OLang 中 `tuple`、`struct`、`fn` 以及后面将要介绍的 `template` 关键字都可以用来创建对应类型的匿名元素，只需要在使用时省略对应的名称部分即可：

```
// 创建匿名元组类型
let Point3D = tuple (i32, i32, i32);

// 创建匿名结构体类型
let Pair = struct .(x = i32, y = i32);

// 创建匿名函数
let foo = fn (x: i32, y: i32) {
    x + y
};
```

特别地，OLang 还支持使用箭头运算符 `->` 创建匿名函数的语法：

```
let foo: i32 -> i32 = x -> x + 1;
let bar = (x: i32, y: i32) -> {
    let c = x + y;
    return c * c;
};
```

#### 模板

模板是 OLang 中进行泛型/模板元编程使用的工具，但因为模板部分的相关设计还未完成，这里展示的模板语法只是一个大致构想，未来仍可能会有较大的改动。

OLang 中模板的定义和使用与函数很像，可以使用 `template` 关键字来创建新的模板对象：

```
// 创建一个泛型 Pair 结构体类型
template Pair (T: type, U: type) struct {
    let first = T;
    let second = U;
}

// 实例化模板类型，模板参数为 T=i32, U=bool
let p = Pair#(i32, bool) .(
    let first = 5,
    let second = TRUE
);
```

虽然 OLang 目前尚未支持模板参数自动推导，但已经为其预留好了语法，实例化模板时省略其后的井号和模板参数即可：

```
let p = Pair .(  // 自动推导类型应为 Pair#(i32, bool)
    let first = 5,
    let second = TRUE
);
```

#### 控制流

OLang 支持其它语言中常见的 `if else`，`while` 和 `for` 这三种流程控制结构，它们都属于表达式，可以对它们进行求值：

```
let word: str = (if (val > 3) "Hello" else "World");
```

可以在 `while` 和 `for` 语句中使用 `break` 返回求值结果，若 `break` 未被执行则求值其后跟随的 `else` 分支并返回其结果：

```
let result = (
    for item_list (it) {
        if (it.getName() == "test_item") {
            break it
        }
    }
    else {
        getDefaultTestItem()
    }
);
```

对于没有 `else` 分支的 `if`/`while`/`for` 语句，OLang 会为它们隐式地补上一个返回空元组的 `else` 分支，因此下面这种写法：

```
let result = (if cond { "Hello" });
```

等同于这种写法：

```
let result = (if cond { "Hello" } else {});  // 别忘了空代码块会返回空元组
```

#### 类型注解

OLang 的类型注解已经在前文的代码示例中出现过，它除了用来标注变量和函数参数的类型外还能用来标注表达式的类型，OLang 会在编译期检查被标注的表达式，确保其推导类型与标注类型相兼容。下面是一些类型注解的使用示例：

```
let a = 5: i32;  // 标注字面量 5 的类型为 i32
let b = a * 7 + 2: i32;  // 标注表达式 a * 7 + 2 的求值结果为 i32 类型
let c = ("Hello": str, b: i32);  // 标注元组中两个元素的类型为 str 和 i32
let d = ("Hello", b): (str, i32);  // 标注整个元组表达式的类型，和上面的写法效果一样
foo(a, b, c): str;  // 标注函数调用的返回类型应为 str
(bar: fn str bool) "Hello";  // 标注被调用函数的类型为 str -> bool
```

#### 语法注解

OLang 的语法注解类似 Java 中提供的注解，它可以为 OLang 代码中的语法元素提供附加信息，以便用户在编译期执行额外的检查，或是提供特殊的功能等。OLang 注解的语法仿照 Java，可以使用 `@` 运算符为代码元素添加标注，OLang 会将 `@` 后的表达式附加到对应的 AST 节点上：

```
// 为 foo 函数添加三个标注
@PureFunction
@Optimize.(frequency=CallFrequency.LOW, force_inline=TRUE)
@ExportFunc("my_foo", ABIType.RUST)
fn foo(x: i32, @Positive y: i32) {  // 为参数 y 添加标注
    return @TestAnnotation (x + y);  // 为表达式 x + y 添加标注
}

@Frequency(BranchFrequency.HIGH)  // 为 if else 条件语句添加标注
if (x > 3) {
    @TestAnnotation (doSomething());  // 标注 doSomething() 函数调用
}
@Frequency(BranchFrequency.MEDIAN)
else if (y == 5) {
    // 请注意，这种写法相当于使用 TestAnnotation(doAnotherThing) 标注空元组
    @TestAnnotation doAnotherThing ();
}
```

### 语义规范

`TODO`

### 参考资料

- pongba 的 [rev#1](https://blog.csdn.net/pongba/article/details/1732055)（[转载](https://jackxiang.com/post/2068/)）和 [rev#2](https://blog.csdn.net/pongba/article/details/1815742)（[转载](https://www.cnblogs.com/taoxu0903/archive/2008/04/04/1137864.html)）博客
- pongba 的[《C++0x漫谈》](https://blog.csdn.net/pongba/category_158724.html)系列博客
- vczh 的[《如何设计一门语言》](http://www.cppblog.com/vczh/archive/2013/04/27/199765.html)系列博客
- 斯坦福大学 [CS143 编译原理](https://www.bilibili.com/video/BV1Mb42177J7)课程
- 加利福尼亚州立大学 [CSC 151 编译器构造](https://www.youtube.com/playlist?list=PL6KMWPQP_DM97Hh0PYNgJord-sANFTI3i)课程
- [Simple but Powerful Pratt Parsing](https://matklad.github.io/2020/04/13/simple-but-powerful-pratt-parsing.html) 短文

### 未来的规划（大饼）

- 设计所有权语法，移除 GC；
- 添加语义检查；
- 支持模板参数推导；
- 完成 OLang 编译器的前端自举；
- 提供类似 Zig 的元编程宏；
- 实现类似 Zig 的编译时惰性分析；
- 支持 C ABI 交互。

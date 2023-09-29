# Disclaimer

В домашних заданиях вы можете менять сигнатуры методов/интерфейс, если на написано обратного. Если в задании нужно
реализовать определенную функцию, которая уже объявлена за вас, в таких случаях ее сигнатуру менять нельзя.

# IO

Реализуйте собственный подсет `IO` функциональности.
`IO` должно быть стэкобезопасным: последовательность вызовов `IO` не должна переполнять стэк.
В этом задании не стоит заботиться об асинхронности, ваш `IO` должен быть полностью синхронным.
Не забудьте про тесты на каждый из реализованных методов. Также, необходим отдельный тест на стекобезопасность.

Суть некоторых из функций:

* `*>[B](another: IO[B]): IO[B]` позволяет выстроить последовательность из `IO` и `another`, которая вернет результат
  выполнения `another`
* `as[B](newValue: => B): IO[B]` заменяет значение, содержащееся в `IO`, на элемент типа `B`
* `attempt` позволяет трансформировать все ошибки `Error`, произошедшие в `IO[A]`, в `IO[Either[Error, A]]`
* `option` заменяет ошибки, произошедшие в `IO`, на None
* `redeem` возвращает новое `IO`, которое трансформирует результат выполнения исходного `IO`: обрабатывает ошибки или
  трансформирует успешное значение
* `redeemWith` то же самое, что и`redeem`, но трансформации несут в себе side эффекты
* `raiseError` возвращает ошибку в `IO`
* `raiseUnless` возвращает `raiseError`, когда `cond = false`, иначе `IO.unit`
* `raiseWhen` возвращает `raiseError`, когда `cond = true`, иначе `IO.unit`
* `unlessA` возвращает переданное `IO`, если `cond = false`, иначе `IO.Unit`
* `whenM` возвращает переданное `IO`, если `cond = true`, иначе `IO.Unit`

Референсы:

* https://typelevel.org/cats-effect/docs/concepts
* https://typelevel.org/cats-effect/api/3.x/cats/effect/IO.html

### Code Style:

Мы последовательно вводим список запрещенных механик, которыми нельзя пользоваться при написании кода, и рекомендаций по
code style. За нарушения мы оставляем за собой право **снижать оценку**.

* Переменные и функции должны иметь осмысленные названия;
* Тест классы именуются `<ClassName>Spec`, где `<ClassName>` - класс к которому пишутся тесты;
* Тест классы находятся в том же пакете, что и класс к которому пишутся тесты (например, класс `Fibonacci` находится в
  пакете `fibonacci` в директории `src/main/scala/fibonacci`, значит его тест класс `FibonacciSpec` должен быть в том же
  пакете в директории `src/test/scala/fibonacci`);
* Каждый тест должен быть в отдельном test suite;
* Использовать java коллекции запрещается (Используйте `Scala` коллекции);
* Использовать `mutable` коллекции запрещается;
* Использовать `var` запрещается;
* Использование `this` запрещается (используйте `self` если требуется);
* Использование `return` запрещается;
* Использование `System.exit` запрещается;
* Касты или проверки на типы с помощью методов из Java вроде `asInstanceOf` запрещаются;
* Использовать `throw` запрещено;
* Использование циклов запрещается (используйте `for comprehension`, `tailRec`, методы `Monad`, `fold`);
* Использование не безопасных вызовов разрешено только в тестах (например `.get` у `Option`);
* Использование взятия и освобождения примитивов синхронизации: semaphore, mutex - из разных потоков запрещено;

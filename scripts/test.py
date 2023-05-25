#!/usr/bin/env python3

from subprocess import run
from unittest import main, TestCase

__unittest = True


class Tests(TestCase):
    def into_test(self, file, expected):
        result = run(
            f"\"$WD\"/scripts/run.sh \"$WD\"/ex/{file}.crl".encode(),
            capture_output=True,
            shell=True,
        )
        self.assertEqual(result.returncode, 0)
        if expected is None:
            return
        self.assertEqual(result.stdout.decode(), f"{expected}\n")

    def test_ackermann_peter(self):
        self.into_test("ackermann_peter", "5, 13, 29, 61, 125")

    def test_channel_send(self):
        self.into_test("channel_send", "\n".join([
            "9",
            "8",
            "7",
            "6",
            "5",
            "4",
            "3",
            "2",
            "1",
            "0",
            "Done!",
        ]))

    def test_collatz(self):
        self.into_test(
            "collatz",
            "9,28,14,7,22,11,34,17,52,26,13,40,20,10,5,16,8,4,2,1",
        )

    def test_dining_philosophers(self):
        self.into_test("dining_philosophers", None)

    def test_divmod(self):
        self.into_test("divmod", "\n".join([
            "2,2",
            "-3,14",
            "-3,-14",
            "2,-2",
            "2,0",
            "-2,0",
            "-2,0",
            "2,0",
            "1,16",
            "-2,2",
            "-2,-2",
            "1,-16",
        ]))

    def test_fib(self):
        self.into_test("fib", 12586269025)

    def test_hello(self):
        self.into_test("hello", "Hello, world!")

    def test_ping_pong(self):
        self.into_test("ping_pong", "\n".join([
            " - ping -",
            " - pong -",
            " - ping -",
            " - pong -",
            " - ping -",
            "Done!",
        ]))

    def test_select(self):
        self.into_test("select", "\n".join([
            " . received `b`",
            " . received `a`",
            "Done!",
        ]))


if __name__ == "__main__":
    main()

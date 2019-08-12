context("written numbers to numerics")

test_that("Numbers to numerics works on a chunk of text", {
example_numerics <- "1213. 133. The PRQ is a 12-item, 4-point Likert scale
(from 1 = Never to 4 = Very Often) with 3 sub-scores
bullying (PRQ-Bully), being victimized (PRQ-Victim),
and pro-social behavior (PRQ-Prosocial). A translated,
backtranslated final Arabic version of the scale was found
to be accurate showing good internal consistency in this
sample [“PRQ-Victim” (alpha = .74)
and “PRQ-Bullies” (alpha = 74)]. 1200. And 12"

example <- "One-thousand, two hundred and thirteen. One-hundred and thirty three. The PRQ is a twelve-item, four-point Likert scale
(from one = Never to four = Very Often) with three sub-scores
bullying (PRQ-Bully), being victimized (PRQ-Victim),
and pro-social behavior (PRQ-Prosocial). A translated,
backtranslated final Arabic version of the scale was found
to be accurate showing good internal consistency in this
sample [“PRQ-Victim” (alpha = .seventy four)
and “PRQ-Bullies” (alpha = seventy-four)]. One thousand two hundred. And twelve"

expect_true(words_to_numbers(example) == example_numerics)
})

test_that("words to numbers works for varied single numer values", {
  expect_identical(words_to_numbers("one hundred"),
                   "100")
  expect_identical(words_to_numbers("one hundred two"),
                   "102")
  expect_identical(words_to_numbers("one hundred and two"),
                   "102")
  expect_identical(words_to_numbers("one hundred and five"),
                   "105")
  expect_identical(words_to_numbers("one hundred and twenty five"),
                   "125")
  expect_identical(words_to_numbers("four thousand and thirty"),
                   "4030")
  expect_identical(words_to_numbers("six million five thousand and two"),
                   "6005002")
  expect_identical(words_to_numbers("six million five thousand and two"),
                   "6005002")
  expect_identical(words_to_numbers("sixty Seven"),
                   "67")
  expect_identical(words_to_numbers("one hundred and sixty seven"),
                   "167")
  expect_identical(words_to_numbers("thousand one hundred and eleven"),
                   "1111")
  expect_identical(words_to_numbers("twenty thousand five hundred and sixty nine"),
                   "20569")
  expect_identical(words_to_numbers("one-hundred and five"),
                   "105")
  expect_identical(words_to_numbers("five quintillion"),
                   "5000000000000000000")
  expect_identical(words_to_numbers("four-thousand and thirty"),
                   "4030")
  expect_identical(words_to_numbers("one-hundred and twenty-Five"),
                   "125")
  expect_identical(words_to_numbers("four-thousand and thirty"),
                   "4030")
  expect_identical(words_to_numbers("six-million five-thousand and two"),
                   "6005002")
  expect_identical(words_to_numbers("one thousand, one-hundred and eleven"),
                   "1111")
  expect_identical(words_to_numbers("thousand, one-hundred and eleven"),
                   "1111")
  expect_identical(words_to_numbers("twenty-thousand, five-hundred and sixty-nine"),
                   "20569")
})

test_that("words to numbers works for multiple numeric values", {
  expect_identical(words_to_numbers("one hundred thousand, hundred"),
                   "100000, 100")
  expect_identical(words_to_numbers("99, 15 thousand"),
                   "99, 15000")
  expect_identical(words_to_numbers("sixteen hundred, twelve thousand and twelve"),
                   "1600, 12012")

})

test_that("words to numbers works with mixed number and non number words", {
  expect_identical(words_to_numbers("there were twenty-thousand, five-hundred and sixty-nine X in the five quintillion Y"),
                   "there were 20569 X in the 5000000000000000000 Y")
    expect_identical(words_to_numbers('1 2 3 one'), "1 2 3 1")
    expect_identical(words_to_numbers('test one two three test'), 'test 1 2 3 test')
    expect_identical(words_to_numbers('digit one'), 'digit 1')
    expect_identical(words_to_numbers('digit one '), 'digit 1 ')
    expect_identical(words_to_numbers('one thirty'), '1 30')
})

test_that("magnitudes alone are read correctly", {
  expect_identical(words_to_numbers('thousand'), "1000")
  expect_identical(words_to_numbers('million'), "1000000")
  expect_identical(words_to_numbers('billion'), "1000000000")
  expect_identical(words_to_numbers('xxxxxxx one hundred'), 'xxxxxxx 100')
})

test_that("non-numerics are left alone", {
  expect_identical(words_to_numbers('and'), 'and')
  expect_identical(words_to_numbers('a'), 'a')
  expect_identical(words_to_numbers('test value'), 'test value')
})

test_that("various odd cases work", {
  expect_identical(words_to_numbers("ten point five, seven point eight"), "10.5, 7.8")
  expect_identical(words_to_numbers('eleven dot one'), "11.1")
  expect_identical(words_to_numbers('Fifty People, One Question Brooklyn'), ('50 People, 1 Question Brooklyn'))
  expect_identical(words_to_numbers('Model Fifty-One Fifty-Six'), ('Model 51 56'))
  expect_identical(words_to_numbers('Fifty Million Frenchmen'), ('50000000 Frenchmen'))
  expect_identical(words_to_numbers('Thousand and One people'), ('1001 people'))
  expect_identical(words_to_numbers('Ten Thousand Pictures'), ('10000 Pictures'))
  expect_identical(words_to_numbers('one thirty'), ("1 30"))
  expect_identical(words_to_numbers('six sixty two'), ("6 62"))
  expect_identical(as.numeric(words_to_numbers('one hundred thousand')),
                   (100000))
  expect_identical(words_to_numbers('I have zero apples and four oranges'), ('I have 0 apples and 4 oranges'))
  expect_identical(words_to_numbers("1000 hundred"), ('1000 100'))
  expect_identical(words_to_numbers("one two three four sixteen hundred twelve thousand and twelve one thousand"),
                   "1 2 3 4 1600 12012 1000")
  expect_identical(words_to_numbers("one two three four sixteen hundred twelve thousand and twelve one thousand, also we have sixty-four dogs"),
                   "1 2 3 4 1600 12012 1000, also we have 64 dogs")
})

test_that("dots are read correctly", {
  expect_identical(words_to_numbers('ten point five'), "10.5")
  expect_identical(words_to_numbers('seventeen dot two four dot twelve'),
                   ('17.2 4.12'))
  expect_identical(words_to_numbers('Dot two Dot'), ('.2 Dot'))
  expect_identical(words_to_numbers('forty two point five'), "42.5")
})

test_that("'twenty one' bug does not reappear '21'", {
  expect_identical(words_to_numbers('twenty one'), ('21'))
  expect_identical(words_to_numbers('20, 1'), ('20, 1'))
})

test_that("Numerics preceeding magnitudes are timesed together", {
  expect_identical(words_to_numbers("16 million"), ("16000000"))
  expect_identical(words_to_numbers('10 thousand'), ("10000"))
  expect_identical(words_to_numbers('10 thousand and 12'), ("10012"))
  expect_identical(words_to_numbers('nineteen eighty thousand'), "19 80000")
  expect_identical(words_to_numbers('one hundred and two thousand'), "102000")
})

test_that("numbers in digits already are not changed", {
  expect_identical(words_to_numbers('1 2 3'), "1 2 3")
  expect_identical(words_to_numbers('sevadfa 10 adfs'), "sevadfa 10 adfs")
  expect_identical(words_to_numbers('sevadfa 10 adfs plus ten'), "sevadfa 10 adfs plus 10")
  expect_identical(words_to_numbers('sevadfa 999,999,999 adfs'), "sevadfa 999,999,999 adfs")
  expect_identical(words_to_numbers('sevadfa 999 to 999 -999 adfs'), 'sevadfa 999 to 999 -999 adfs')
  expect_identical(words_to_numbers('sevadfa -10 adfs'), "sevadfa -10 adfs")
  expect_identical(words_to_numbers('sevadfa .30 adfs'), "sevadfa .30 adfs")
})

test_that("We do not error out to 0s", {
  expect_identical(as.numeric(words_to_numbers("sixty hundred thousand")),
                   6e+06)
})

 test_that("nineteen eighty thousand", {
     expect_identical(words_to_numbers('nineteen eighty thousand'), ('19 80000'))
   })

 test_that("TENS to UNITS > 10 breaks", {
   expect_identical(words_to_numbers('thirty eleven'), ('30 11'))
   expect_identical(words_to_numbers('twenty nineteen'), ('20 19'))
})


 test_that("incorrect objects kick up warnings", {
  expect_warning(words_to_numbers(c("one two", "three")))
  expect_warning(words_to_numbers(list("one two", "three")))
  expect_warning(words_to_numbers(data.frame("one two", "three")))
 }
)

 test_that("Decimals to magnitudes works", {
   expect_identical(words_to_numbers('1.6 thousand'), ('1600'))
   expect_identical(words_to_numbers('3.5 million'), ('3500000'))
   expect_identical(words_to_numbers('1.8 hundred thousand'), ('180000'))
 })

 test_that("compound words with decimal magnitudes", {
   expect_identical(words_to_numbers('3.56 million, one million'), ('3560000, 1000000'))
   expect_identical(words_to_numbers('1.8 million thousand'), ('1800000 1000'))
   expect_identical(words_to_numbers('1.8 million, thousand'), ('1800000, 1000'))
   expect_identical(words_to_numbers('1.81 million one hundred'), ('1810100'))
   expect_identical(words_to_numbers('1.8 million one hundred and twelve'), ('1800112'))
   expect_identical(words_to_numbers('1.8 million one hundred and 12'), ('1800112'))
   expect_identical(words_to_numbers("million three million"), "1000000 3000000")
 })


#  the below illustrate cases where slightly non-intuitive outcomes results are returned
# The optimal returns are shown,
# test_that("one thirty thousand", {
#     expect_identical(words_to_numbers('one thirty thousand'),
#                      ("130000"))
#   })
#
#
# Notably we only capture one number after the decimal
# test_that("three point one four one five nine two six", {
#   expect_identical(words_to_numbers('three point one four one five nine two six'),
#                    3.1415926)
# })
#
#
#  And word splitting breaks if the reason for the split is more than 3 numeric tokens apart
# test_that("three point one four one five nine two six", {
#   expect_identical(words_to_numbers("two thousand and twenty-five million"),
#                    "2000 and 25000000")
# })
#
# Some weird things can also happen if we have decimal magnitudes followed by values
# test_that("three point one four one five nine two six", {
#   expect_identical(words_to_numbers("1.232001 million and 12"),
#                    "1232001 and 12")
# })
#
#

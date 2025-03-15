module Export.MarkdownTests exposing (suite)

import Expect
import Export.Markdown
import Test exposing (Test, describe, test)
import TestData


crlf : String
crlf =
    "\u{000D}\n"


suite : Test
suite =
    describe "The Export.Markdown module"
        [ test "can format a glossary as Markdown" <|
            \_ ->
                let
                    expected : String
                    expected =
                        """
# Example Glossary

An example glossary.

---------

## Tags

* Computer Science — These are items about computer science — the study of computation, information, and automation.
* Finance — These are items about finance — the study and discipline of money, currency and capital assets.
* Gardening — These are items about gardening — the practice of growing and cultivating plants as part of horticulture.

## Items

**Default (Computer Science)**\\
**Preset**\\
**Factory preset**

[Tags: Computer Science]

The preexisting value of a user-configurable setting that is assigned to a software application, computer program or device. Such settings are also called presets or factory presets, especially for electronic devices.

**Default (Finance)**

[Tags: Finance]

In finance, default is failure to meet the legal obligations (or conditions) of a loan, for example when a home buyer fails to make a mortgage payment, or when a corporation or government fails to pay a bond which has reached maturity. A national or sovereign default is the failure or refusal of a government to repay its national debt.

See also: Loan

**Information retrieval**\\
**IR**

[Tags: Computer Science]

Information retrieval (IR) in computing and information science is the process of obtaining information system resources that are relevant to an information need from a collection of those resources. Searches can be based on full-text or other content-based indexing.

**Interest rate**\\
**IR**

[Tags: Finance]

An interest rate is the amount of interest due per period, as a proportion of the amount lent, deposited, or borrowed (called the principal sum). The total interest on an amount lent or borrowed depends on the principal sum, the interest rate, the compounding frequency, and the length of time over which it is lent, deposited, or borrowed.

See also: Loan

**Loan**

[Tags: Finance]

The transfer of money by one party to another with an agreement to pay it back. The recipient, or borrower, incurs a debt and is usually required to pay interest for the use of the money.

See also: Interest rate"""
                            |> String.trim
                            |> String.replace "\n" crlf
                in
                TestData.glossaryForUi
                    |> Export.Markdown.toString
                    |> Expect.equal expected
        ]

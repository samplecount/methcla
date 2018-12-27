/*
 *  Created by Phil on 5/12/2012.
 *  Copyright 2012 Two Blue Cubes Ltd. All rights reserved.
 *
 *  Distributed under the Boost Software License, Version 1.0. (See accompanying
 *  file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
 */
#ifndef METHCLA_CATCH_CALLBACK_REPORTER_HPP_INCLUDED
#define METHCLA_CATCH_CALLBACK_REPORTER_HPP_INCLUDED

#include <functional>
#include <sstream>

#include <catch.hpp>

namespace Catch {

    struct CallbackReporter : StreamingReporterBase
    {
        typedef std::function<void(const std::string&)> Callback;

        CallbackReporter(ReporterConfig const& _config, Callback callback)
        : StreamingReporterBase(_config)
        , m_callback(callback)
        , m_headerPrinted(false)
        , m_atLeastOneTestCasePrinted(false)
        {}

        static std::string getDescription()
        {
            return "Reports test results as plain lines of text";
        }

        virtual ReporterPreferences getPreferences() const
        {
            ReporterPreferences prefs;
            prefs.shouldRedirectStdOut = false;
            return prefs;
        }

        virtual void noMatchingTestCases(std::string const& spec)
        {
            m_stream << "No test cases matched '" << spec << "'" << std::endl;
            flush();
        }

        virtual void assertionStarting(AssertionInfo const&) {}

        virtual bool assertionEnded(AssertionStats const& _assertionStats)
        {
            AssertionResult const& result = _assertionStats.assertionResult;

            bool printInfoMessages = true;

            // Drop out if result was successful and we're not printing those
            if (!m_config->includeSuccessfulResults() && result.isOk())
            {
                if (result.getResultType() != ResultWas::Warning)
                    return false;
                printInfoMessages = false;
            }

            lazyPrint();

            AssertionPrinter printer(m_stream, _assertionStats,
                                     printInfoMessages);
            printer.print();
            m_stream << std::endl;

            flush();

            return true;
        }

        virtual void sectionStarting(SectionInfo const& _sectionInfo)
        {
            m_headerPrinted = false;
            StreamingReporterBase::sectionStarting(_sectionInfo);
            flush();
        }

        virtual void sectionEnded(SectionStats const& _sectionStats)
        {
            if (_sectionStats.missingAssertions)
            {
                lazyPrint();
                Colour colour(Colour::ResultError);
                if (m_sectionStack.size() > 1)
                    m_stream << "\nNo assertions in section";
                else
                    m_stream << "\nNo assertions in test case";
                m_stream << " '" << _sectionStats.sectionInfo.name << "'\n"
                         << std::endl;
            }
            if (m_headerPrinted)
            {
                if (m_config->showDurations() == ShowDurations::Always)
                    m_stream << "Completed in "
                             << _sectionStats.durationInSeconds << "s"
                             << std::endl;
                m_headerPrinted = false;
            }
            else
            {
                if (m_config->showDurations() == ShowDurations::Always)
                    m_stream
                        << _sectionStats.sectionInfo.name << " completed in "
                        << _sectionStats.durationInSeconds << "s" << std::endl;
            }
            StreamingReporterBase::sectionEnded(_sectionStats);
            flush();
        }

        virtual void testCaseEnded(TestCaseStats const& _testCaseStats)
        {
            StreamingReporterBase::testCaseEnded(_testCaseStats);
            m_headerPrinted = false;
            flush();
        }

        virtual void testGroupEnded(TestGroupStats const& _testGroupStats)
        {
            if (currentGroupInfo.used)
            {
                printSummaryDivider();
                m_stream << "Summary for group '"
                         << _testGroupStats.groupInfo.name << "':\n";
                printTotals(_testGroupStats.totals);
                m_stream << "\n" << std::endl;
            }
            StreamingReporterBase::testGroupEnded(_testGroupStats);
            flush();
        }

        virtual void testRunEnded(TestRunStats const& _testRunStats)
        {
            if (m_atLeastOneTestCasePrinted)
                printTotalsDivider();
            printTotals(_testRunStats.totals);
            m_stream << "\n" << std::endl;
            StreamingReporterBase::testRunEnded(_testRunStats);
            flush();
        }

        void flush()
        {
            m_callback(m_stream.str());
            m_stream.str(std::string());
        }

    private:
        class AssertionPrinter
        {
            void operator=(AssertionPrinter const&);

        public:
            AssertionPrinter(std::ostream&         _stream,
                             AssertionStats const& _stats,
                             bool                  _printInfoMessages)
            : stream(_stream)
            , stats(_stats)
            , result(_stats.assertionResult)
            , colour(Colour::None)
            , message(result.getMessage())
            , messages(_stats.infoMessages)
            , printInfoMessages(_printInfoMessages)
            {
                switch (result.getResultType())
                {
                    case ResultWas::Ok:
                        colour = Colour::Success;
                        passOrFail = "PASSED";
                        // if( result.hasMessage() )
                        if (_stats.infoMessages.size() == 1)
                            messageLabel = "with message";
                        if (_stats.infoMessages.size() > 1)
                            messageLabel = "with messages";
                        break;
                    case ResultWas::ExpressionFailed:
                        if (result.isOk())
                        {
                            colour = Colour::Success;
                            passOrFail = "FAILED - but was ok";
                        }
                        else
                        {
                            colour = Colour::Error;
                            passOrFail = "FAILED";
                        }
                        if (_stats.infoMessages.size() == 1)
                            messageLabel = "with message";
                        if (_stats.infoMessages.size() > 1)
                            messageLabel = "with messages";
                        break;
                    case ResultWas::ThrewException:
                        colour = Colour::Error;
                        passOrFail = "FAILED";
                        messageLabel =
                            "due to unexpected exception with message";
                        break;
                    case ResultWas::DidntThrowException:
                        colour = Colour::Error;
                        passOrFail = "FAILED";
                        messageLabel =
                            "because no exception was thrown where one "
                            "was expected";
                        break;
                    case ResultWas::Info:
                        messageLabel = "info";
                        break;
                    case ResultWas::Warning:
                        messageLabel = "warning";
                        break;
                    case ResultWas::ExplicitFailure:
                        passOrFail = "FAILED";
                        colour = Colour::Error;
                        if (_stats.infoMessages.size() == 1)
                            messageLabel = "explicitly with message";
                        if (_stats.infoMessages.size() > 1)
                            messageLabel = "explicitly with messages";
                        break;
                    // These cases are here to prevent compiler warnings
                    case ResultWas::Unknown:
                    case ResultWas::FailureBit:
                    case ResultWas::Exception:
                        passOrFail = "** internal error **";
                        colour = Colour::Error;
                        break;
                }
            }

            void print() const
            {
                printSourceInfo();
                if (stats.totals.assertions.total() > 0)
                {
                    if (result.isOk())
                        stream << "\n";
                    printResultType();
                    printOriginalExpression();
                    printReconstructedExpression();
                }
                else
                {
                    stream << "\n";
                }
                printMessage();
            }

        private:
            void printResultType() const
            {
                if (!passOrFail.empty())
                {
                    Colour colourGuard(colour);
                    stream << passOrFail << ":\n";
                }
            }
            void printOriginalExpression() const
            {
                if (result.hasExpression())
                {
                    Colour colourGuard(Colour::OriginalExpression);
                    stream << "  ";
                    stream << result.getExpressionInMacro();
                    stream << "\n";
                }
            }
            void printReconstructedExpression() const
            {
                if (result.hasExpandedExpression())
                {
                    stream << "with expansion:\n";
                    Colour colourGuard(Colour::ReconstructedExpression);
                    stream << Text(result.getExpandedExpression(),
                                   TextAttributes().setIndent(2))
                           << "\n";
                }
            }
            void printMessage() const
            {
                if (!messageLabel.empty())
                    stream << messageLabel << ":"
                           << "\n";
                for (std::vector<MessageInfo>::const_iterator
                         it = messages.begin(),
                         itEnd = messages.end();
                     it != itEnd; ++it)
                {
                    // If this assertion is a warning ignore any INFO messages
                    if (printInfoMessages || it->type != ResultWas::Info)
                        stream
                            << Text(it->message, TextAttributes().setIndent(2))
                            << "\n";
                }
            }
            void printSourceInfo() const
            {
                Colour colourGuard(Colour::FileName);
                stream << result.getSourceInfo() << ": ";
            }

            std::ostream&            stream;
            AssertionStats const&    stats;
            AssertionResult const&   result;
            Colour::Code             colour;
            std::string              passOrFail;
            std::string              messageLabel;
            std::string              message;
            std::vector<MessageInfo> messages;
            bool                     printInfoMessages;
        };

        void lazyPrint()
        {

            if (!currentTestRunInfo.used)
                lazyPrintRunInfo();
            if (!currentGroupInfo.used)
                lazyPrintGroupInfo();

            if (!m_headerPrinted)
            {
                printTestCaseAndSectionHeader();
                m_headerPrinted = true;
            }
            m_atLeastOneTestCasePrinted = true;
        }
        void lazyPrintRunInfo()
        {
            m_stream << "\n" << getTildes() << "\n";
            Colour colour(Colour::SecondaryText);
            m_stream << currentTestRunInfo->name << " is a Catch v"
                     << libraryVersion.majorVersion << "."
                     << libraryVersion.minorVersion << " b"
                     << libraryVersion.buildNumber;
            if (libraryVersion.branchName != "master")
                m_stream << " (" << libraryVersion.branchName << ")";
            m_stream << " host application.\n"
                     << "Run with -? for options\n\n";

            currentTestRunInfo.used = true;
        }
        void lazyPrintGroupInfo()
        {
            if (!currentGroupInfo->name.empty() &&
                currentGroupInfo->groupsCounts > 1)
            {
                printClosedHeader("Group: " + currentGroupInfo->name);
                currentGroupInfo.used = true;
            }
        }
        void printTestCaseAndSectionHeader()
        {
            assert(!m_sectionStack.empty());
            printOpenHeader(currentTestCaseInfo->name);

            if (m_sectionStack.size() > 1)
            {
                Colour colourGuard(Colour::Headers);

                std::vector<SectionInfo>::const_iterator
                    it = m_sectionStack.begin() +
                         1, // Skip first section (test case)
                    itEnd = m_sectionStack.end();
                for (; it != itEnd; ++it)
                    printHeaderString(it->name, 2);
            }

            SourceLineInfo lineInfo = m_sectionStack.front().lineInfo;

            if (!lineInfo.empty())
            {
                m_stream << getDashes() << "\n";
                Colour colourGuard(Colour::FileName);
                m_stream << lineInfo << "\n";
            }
            m_stream << getDots() << "\n" << std::endl;
        }

        void printClosedHeader(std::string const& _name)
        {
            printOpenHeader(_name);
            m_stream << getDots() << "\n";
        }
        void printOpenHeader(std::string const& _name)
        {
            m_stream << getDashes() << "\n";
            {
                Colour colourGuard(Colour::Headers);
                printHeaderString(_name);
            }
        }

        // if string has a : in first line will set indent to follow it on
        // subsequent lines
        void printHeaderString(std::string const& _string,
                               std::size_t        indent = 0)
        {
            std::size_t i = _string.find(": ");
            if (i != std::string::npos)
                i += 2;
            else
                i = 0;
            m_stream << Text(_string, TextAttributes()
                                          .setIndent(indent + i)
                                          .setInitialIndent(indent))
                     << "\n";
        }

        void printTotals(const Totals& totals)
        {
            if (totals.testCases.total() == 0)
            {
                m_stream << "No tests ran";
            }
            else if (totals.assertions.total() == 0)
            {
                Colour colour(Colour::Yellow);
                printCounts("test case", totals.testCases);
                m_stream << " (no assertions)";
            }
            else if (totals.assertions.failed)
            {
                Colour colour(Colour::ResultError);
                printCounts("test case", totals.testCases);
                if (totals.testCases.failed > 0)
                {
                    m_stream << " (";
                    printCounts("assertion", totals.assertions);
                    m_stream << ")";
                }
            }
            else
            {
                Colour colour(Colour::ResultSuccess);
                m_stream << "All tests passed ("
                         << pluralise(totals.assertions.passed, "assertion")
                         << " in "
                         << pluralise(totals.testCases.passed, "test case")
                         << ")";
            }
        }
        void printCounts(std::string const& label, Counts const& counts)
        {
            if (counts.total() == 1)
            {
                m_stream << "1 " << label << " - ";
                if (counts.failed)
                    m_stream << "failed";
                else
                    m_stream << "passed";
            }
            else
            {
                m_stream << counts.total() << " " << label << "s ";
                if (counts.passed)
                {
                    if (counts.failed)
                        m_stream << "- " << counts.failed << " failed";
                    else if (counts.passed == 2)
                        m_stream << "- both passed";
                    else
                        m_stream << "- all passed";
                }
                else
                {
                    if (counts.failed == 2)
                        m_stream << "- both failed";
                    else
                        m_stream << "- all failed";
                }
            }
        }

        void printTotalsDivider() { m_stream << getDoubleDashes() << "\n"; }
        void printSummaryDivider() { m_stream << getDashes() << "\n"; }
        static std::string const& getDashes()
        {
            static const std::string dashes(CATCH_CONFIG_CONSOLE_WIDTH - 1,
                                            '-');
            return dashes;
        }
        static std::string const& getDots()
        {
            static const std::string dots(CATCH_CONFIG_CONSOLE_WIDTH - 1, '.');
            return dots;
        }
        static std::string const& getDoubleDashes()
        {
            static const std::string doubleDashes(
                CATCH_CONFIG_CONSOLE_WIDTH - 1, '=');
            return doubleDashes;
        }
        static std::string const& getTildes()
        {
            static const std::string dots(CATCH_CONFIG_CONSOLE_WIDTH - 1, '~');
            return dots;
        }

    private:
        Callback          m_callback;
        std::stringstream m_stream;
        bool              m_headerPrinted;
        bool              m_atLeastOneTestCasePrinted;
    };

    class CallbackReporterRegistrar
    {

        class ReporterFactory : public IReporterFactory
        {
            CallbackReporter::Callback m_callback;

        public:
            ReporterFactory(CallbackReporter::Callback callback)
            : m_callback(callback)
            {}

        private:
            virtual IStreamingReporter*
            create(ReporterConfig const& config) const
            {
                return new CallbackReporter(config, m_callback);
            }

            virtual std::string getDescription() const
            {
                return CallbackReporter::getDescription();
            }
        };

    public:
        CallbackReporterRegistrar(std::string const&         name,
                                  CallbackReporter::Callback callback)
        {
            getMutableRegistryHub().registerReporter(
                name, new ReporterFactory(callback));
        }
    };

    inline static void
    registerCallbackReporter(const std::string&         name,
                             CallbackReporter::Callback callback)
    {
        // INTERNAL_CATCH_REGISTER_REPORTER( name, CallbackReporter(callback) );
        new Catch::CallbackReporterRegistrar(name, callback);
    }
} // end namespace Catch

#endif // METHCLA_CATCH_CALLBACK_REPORTER_HPP_INCLUDED

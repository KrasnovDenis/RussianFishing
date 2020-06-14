package com.russianfishing.net;

import com.ugos.jiprolog.engine.*;

import java.io.File;

public class PrologEventListener implements JIPEventListener {
    private final PrologProcessor processor;
    private int m_nQueryHandle;
    private final boolean end = false;

    PrologEventListener(PrologProcessor processor) {
        this.processor = processor;
    }

    @Override
    public void solutionNotified(JIPEvent jipEvent) {
        StringBuilder res = new StringBuilder();
        synchronized (jipEvent.getSource()) {
            JIPTerm solution = jipEvent.getTerm();
            JIPVariable[] vars = solution.getVariables();
            for (JIPVariable var : vars) {
                if (!var.isAnonymous()) {
                    res.append(var.toString(jipEvent.getSource())).append('\n');
                }
            }
            processor.setAnswer(TranslateUtils.clean(res.toString()));
            jipEvent.getSource().nextSolution(jipEvent.getQueryHandle());
        }
    }

    public synchronized void start(String request) {
        JIPEngine jip = new JIPEngine();

        jip.addEventListener(this);

        try {
            File file = new File(getClass().getResource("/prolog/test.pl").getFile());
            jip.consultFile(file.getAbsolutePath());
        } catch (JIPSyntaxErrorException ex) {
            ex.printStackTrace();
        }

        JIPTerm query = null;

        try {
            request = TranslateUtils.translateToLatin(request.toLowerCase());
            query = jip.getTermParser().parseTerm("eliza(\"" + request + "\", X)." );
        } catch (JIPSyntaxErrorException ex) {
            ex.printStackTrace();
            System.exit(0);
        }

        synchronized (jip) {
            m_nQueryHandle = jip.openQuery(query);
        }

        if (!end) {
            try {
                wait();
            } catch (InterruptedException e) {
                // TODO Auto-generated catch block
                e.printStackTrace();
            }
        }
    }

    // open event occurred
    @Override
    public void openNotified(JIPEvent e) {
        // syncronization is required to avoid that listeners is
        // called before the method openQuery returns the handle.
        synchronized (e.getSource()) {
            if (m_nQueryHandle == e.getQueryHandle()) {
                System.out.println("open");
            }
        }
    }

    // more event occurred
    @Override
    public void moreNotified(JIPEvent e) {
        // syncronization is required to avoid that listeners is
        // called before the method openQuery returns the handle.
        synchronized (e.getSource()) {
            if (m_nQueryHandle == e.getQueryHandle()) {
                System.out.println("more");
            }
        }
    }


    // A Term has been notified with notify/2
    @Override
    public void termNotified(JIPEvent e) {
        synchronized (e.getSource()) {
            if (m_nQueryHandle == e.getQueryHandle()) {
                System.out.println("term " + e.getTerm());
            }
        }
    }

    // The end has been reached because there wasn't more solutions
    @Override
    public synchronized void endNotified(JIPEvent e) {
        synchronized (e.getSource()) {
            if (m_nQueryHandle == e.getQueryHandle()) {
                System.out.println("end");

                // get the source of the query
                JIPEngine jip = e.getSource();

                // close query
                jip.closeQuery(m_nQueryHandle);
            }
        }

        // notify end
        notify();
    }

    @Override
    public synchronized void closeNotified(JIPEvent e) {
        synchronized (e.getSource()) {
            if (m_nQueryHandle == e.getQueryHandle()) {
                System.out.println("close");
            }
        }

        // notify end
        notify();
    }

    // An error (exception) has been raised up by prolog engine
    @Override
    public synchronized void errorNotified(JIPErrorEvent e) {
        synchronized (e.getSource()) {
            if (m_nQueryHandle == e.getQueryHandle()) {
                System.out.println("Error:");
                System.out.println(e.getError());

                // get the source of the query
                JIPEngine jip = e.getSource();

                // close query
                jip.closeQuery(m_nQueryHandle);
            }
        }

        // notify end
        notify();
    }
}
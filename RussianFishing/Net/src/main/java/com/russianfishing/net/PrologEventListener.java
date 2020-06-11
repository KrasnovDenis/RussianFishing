package com.russianfishing.net;

import com.ugos.jiprolog.engine.*;

import java.io.File;
import java.io.IOException;

public class PrologEventListener implements JIPEventListener {
    private final PrologProcessor processor;
    private int m_nQueryHandle;
    private boolean end = false;

    PrologEventListener(PrologProcessor processor) {
        this.processor = processor;
    }

    @Override
    public void solutionNotified(JIPEvent jipEvent) {
        String res = "";
        synchronized (jipEvent.getSource()) {
            JIPTerm solution = jipEvent.getTerm();
            JIPVariable[] vars = solution.getVariables();
            for (JIPVariable var : vars) {
                if (!var.isAnonymous()) {
                    res = res + (var.getName() + " = " + var.toString(jipEvent.getSource()) + " ") + '\n';
                }
            }
            processor.setAnswer(res);
            jipEvent.getSource().nextSolution(jipEvent.getQueryHandle());
        }
    }

    public synchronized void start(String request) {
        // New instance of prolog engine
        JIPEngine jip = new JIPEngine();

        // add listeners
        jip.addEventListener(this);

        // consult file
        try {
            File file = new File(getClass().getResource("/prolog/eliza.pl").getFile());
            // consult file
            jip.consultFile(file.getAbsolutePath());
        } catch (JIPSyntaxErrorException ex) {
            ex.printStackTrace();
        }

        JIPTerm query = null;

        // parse query
        try {
            request = request.toLowerCase();
            query = jip.getTermParser().parseTerm("eliza(\"" + request + "\", X)." );
        } catch (JIPSyntaxErrorException ex) {
            ex.printStackTrace();
            System.exit(0);
        }

        // open Query
        synchronized (jip) {
            // It's better to have the first call under syncronization
            // to avoid that listeners is called before the method
            // openQuery returns the handle.
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
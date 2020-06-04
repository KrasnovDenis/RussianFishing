package com.russianfishing.net;

import com.ugos.jiprolog.engine.JIPEngine;
import com.ugos.jiprolog.engine.JIPSyntaxErrorException;
import com.ugos.jiprolog.engine.JIPTerm;
import org.springframework.stereotype.Service;

@Service
public class ElizaService {
    private boolean end = false;
    private int m_nQueryHandle;

    public static void main(String[] args) {
        ElizaService elizaService = new ElizaService();
        System.out.println(elizaService.execute("denis"));

    }
    public String execute(String request) {
        // consult file
        JIPEngine jip = new JIPEngine();
        PrologEventListener listener = new PrologEventListener();
        try {
            jip.addEventListener(listener);
            jip.consultFile("\\RussianFishing\\Net\\src\\main\\resources\\prolog\\familyrelationships.pl");

        } catch (JIPSyntaxErrorException ex) {
            ex.printStackTrace();
        }

        JIPTerm query = null;

        // parse query
        try {
            query = jip.getTermParser().parseTerm("father(X, " + request + ")");
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
        try {
            wait(600);
        } catch (InterruptedException e) {
            e.printStackTrace();
        }

        return listener.getRes();
    }

}

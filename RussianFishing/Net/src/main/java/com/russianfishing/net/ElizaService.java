package com.russianfishing.net;

import com.ugos.jiprolog.engine.JIPEngine;
import com.ugos.jiprolog.engine.JIPSyntaxErrorException;
import com.ugos.jiprolog.engine.JIPTerm;
import org.springframework.stereotype.Service;

@Service
public class ElizaService {
    public String execute(String request) {
        PrologProcessor processor = new PrologProcessor();
        PrologEventListener listener = new PrologEventListener(processor);
        listener.start(request);

        return processor.getAnswer();
    }
}

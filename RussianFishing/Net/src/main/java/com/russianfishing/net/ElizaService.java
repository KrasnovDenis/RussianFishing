package com.russianfishing.net;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

@Service
public class ElizaService {


    private final PrologEventListener listener;
    private final PrologProcessor prologProcessor;

    @Autowired
    public ElizaService(PrologEventListener listener, PrologProcessor prologProcessor) {
        this.listener = listener;
        this.prologProcessor = prologProcessor;
    }


    public String execute(String request) {
        listener.start(request);
        return prologProcessor.getAnswer();
    }
}

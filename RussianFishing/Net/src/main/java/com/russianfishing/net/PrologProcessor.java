package com.russianfishing.net;

import org.springframework.stereotype.Service;

@Service
public class PrologProcessor {

    private String answer;

    public String getAnswer() {
        return answer;
    }

    public void setAnswer(String answer) {
        this.answer = answer;
    }
}

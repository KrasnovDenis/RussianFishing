package com.russianfishing.net;

import com.fasterxml.jackson.annotation.JsonInclude;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.MediaType;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.ResponseBody;

@JsonInclude(JsonInclude.Include.NON_NULL)
@Controller
public class MainController {

    private final ElizaService service;

    @Autowired
    public MainController(ElizaService service) {
        this.service = service;
    }

    @RequestMapping(value = "/chatroom", method = RequestMethod.GET)
    public String chatPageShow(Model model) {
        return "chatroom";
    }

    @RequestMapping(value = {"/", "/index"}, method = RequestMethod.GET)
    public String welcomePage(Model model) {
        return "index";
    }

    @RequestMapping(value = "/chatroom", method = RequestMethod.POST, produces = MediaType.TEXT_PLAIN_VALUE)
    @ResponseBody
    public String getMessage(@RequestBody String message) {
        return service.execute(message);
    }
}

package me.excel.tools.prompter;

import java.util.ArrayList;
import java.util.List;

/**
 * Created by hanwen on 2016/12/19.
 */
public class PromptBuilder {

  private List<FieldPrompter> prompterList = new ArrayList<>();

  public FieldPrompter[] build() {
    return prompterList.toArray(new FieldPrompter[0]);
  }

  public PromptBuilder add(FieldPrompter prompter) {
    prompterList.add(prompter);
    return this;
  }

  public PromptBuilder prompt(String field, String prompt) {
    prompterList.add(new DefaultFieldPrompter(field, prompt));
    return this;
  }
}

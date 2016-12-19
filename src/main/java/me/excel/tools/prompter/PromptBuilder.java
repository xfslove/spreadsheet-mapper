package me.excel.tools.prompter;

import java.util.ArrayList;
import java.util.List;

/**
 * Created by hanwen on 2016/12/19.
 */
public class PromptBuilder {

  private List<CellPrompter> prompterList = new ArrayList<>();

  public CellPrompter[] build() {
    return prompterList.toArray(new CellPrompter[0]);
  }

  public PromptBuilder add(CellPrompter prompter) {
    prompterList.add(prompter);
    return this;
  }

  public PromptBuilder prompt(String field, String prompt) {
    prompterList.add(new DefaultCellPrompter(field, prompt));
    return this;
  }
}

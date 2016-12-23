package me.excel.tools.prompter;

import java.util.ArrayList;
import java.util.List;

/**
 * prompt builder to create field prompt
 * <p>
 * Created by hanwen on 2016/12/19.
 */
public class PromptBuilder {

  private List<FieldPrompter> prompterList = new ArrayList<>();

  public FieldPrompter[] build() {
    return prompterList.toArray(new FieldPrompter[0]);
  }

  /**
   * add prompt
   *
   * @param prompter
   * @return
   */
  public PromptBuilder add(FieldPrompter prompter) {
    prompterList.add(prompter);
    return this;
  }

  /**
   * add prompt
   *
   * @param field
   * @param prompt
   * @return
   */
  public PromptBuilder prompt(String field, String prompt) {
    prompterList.add(new DefaultFieldPrompter(field, prompt));
    return this;
  }

  /**
   * add prompt
   *
   * @param field
   * @param prompts
   * @return
   */
  public PromptBuilder prompt(String field, String... prompts) {
    if (prompts == null) {
      return this;
    }
    for (String prompt : prompts) {
      prompterList.add(new DefaultFieldPrompter(field, prompt));
    }
    return this;
  }
}

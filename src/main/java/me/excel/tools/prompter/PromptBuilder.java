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

  /**
   * @return get field prompters
   */
  public FieldPrompter[] build() {
    return prompterList.toArray(new FieldPrompter[0]);
  }

  /**
   * add prompt
   *
   * @param prompter prompter
   * @return this
   */
  public PromptBuilder add(FieldPrompter prompter) {
    prompterList.add(prompter);
    return this;
  }

  /**
   * add prompt
   *
   * @param field  field
   * @param prompt prompt string
   * @return this
   */
  public PromptBuilder prompt(String field, String prompt) {
    prompterList.add(new DefaultFieldPrompter(field, prompt));
    return this;
  }

  /**
   * add prompt
   *
   * @param field   field
   * @param prompts prompt strings
   * @return this
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

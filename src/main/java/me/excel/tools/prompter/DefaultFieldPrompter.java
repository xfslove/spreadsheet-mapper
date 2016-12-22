package me.excel.tools.prompter;

/**
 * default prompter
 * <p>
 * Created by hanwen on 2016/12/19.
 */
public class DefaultFieldPrompter implements FieldPrompter {

  private String matchField;

  private String prompt;

  public DefaultFieldPrompter(String matchField, String prompt) {
    this.matchField = matchField;
    this.prompt = prompt;
  }

  protected final String getMatchField() {
    return matchField;
  }

  @Override
  public boolean matches(String field) {
    return matchField.equals(field);
  }

  @Override
  public String getPrompt() {
    return prompt;
  }
}

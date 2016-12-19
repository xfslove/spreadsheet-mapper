package me.excel.tools.prompter;

/**
 * Created by hanwen on 2016/12/19.
 */
public class DefaultCellPrompter implements CellPrompter {

  private String matchField;

  private String prompt;

  public DefaultCellPrompter(String matchField, String prompt) {
    this.matchField = matchField;
    this.prompt = prompt;
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

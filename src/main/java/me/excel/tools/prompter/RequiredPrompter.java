package me.excel.tools.prompter;

/**
 * required prompter
 * <p>
 * Created by hanwen on 2016/12/19.
 */
public class RequiredPrompter extends DefaultFieldPrompter {

  public RequiredPrompter(String matchField) {
    super(matchField, "必填");
  }
}

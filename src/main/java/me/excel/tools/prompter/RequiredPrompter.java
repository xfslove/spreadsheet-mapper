package me.excel.tools.prompter;

/**
 * 必填提示器
 * Created by hanwen on 2016/12/19.
 */
public class RequiredPrompter extends DefaultCellPrompter {

  public RequiredPrompter(String matchField) {
    super(matchField, "必填");
  }
}

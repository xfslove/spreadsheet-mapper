package me.excel.tools.prompter;

/**
 * field prompter
 * Created by hanwen on 2016/12/19.
 */
public interface FieldPrompter {

  /**
   * get field prompter
   *
   * @return
   */
  String getPrompt();

  /**
   * matches which field
   *
   * @param field
   * @return
   */
  boolean matches(String field);
}

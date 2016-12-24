package me.excel.tools.prompter;

/**
 * field prompter
 * Created by hanwen on 2016/12/19.
 */
public interface FieldPrompter {

  /**
   * @return field prompter
   */
  String getPrompt();

  /**
   * matches which field
   *
   * @param field field
   * @return success
   */
  boolean matches(String field);
}

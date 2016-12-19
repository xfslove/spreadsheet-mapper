package me.excel.tools.prompter;

/**
 * prompter
 * Created by hanwen on 2016/12/19.
 */
public interface CellPrompter {

  String getPrompt();

  boolean matches(String field);
}

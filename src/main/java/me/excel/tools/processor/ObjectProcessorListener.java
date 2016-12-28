package me.excel.tools.processor;

import java.util.List;

/**
 * listener of workbook to object processor
 * <p>
 * Created by hanwen on 15-12-16.
 */
public interface ObjectProcessorListener<OBJECT> {

  /**
   * before object value set
   *
   * @param model value not set
   */
  void beforeRow(OBJECT model);

  /**
   * after object value set
   *
   * @param model value set
   */
  void afterRow(OBJECT model);

  /**
   * after all object value set
   *
   * @param models value set model list
   */
  void afterSheet(List<OBJECT> models);
}

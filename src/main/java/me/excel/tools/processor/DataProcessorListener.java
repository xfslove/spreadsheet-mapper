package me.excel.tools.processor;

import java.util.List;

/**
 * listener of workbook to object processor
 * <p>
 * Created by hanwen on 15-12-16.
 */
public interface DataProcessorListener {

  /**
   * before object value set
   *
   * @param model value not set
   */
  void beforeRow(Object model);

  /**
   * after object value set
   *
   * @param model value set
   */
  void afterRow(Object model);

  /**
   * after all object value set
   *
   * @param models value set model list
   */
  void afterSheet(List models);
}

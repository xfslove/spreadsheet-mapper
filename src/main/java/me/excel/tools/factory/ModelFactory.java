package me.excel.tools.factory;

import me.excel.tools.model.excel.ExcelRow;

/**
 * model factory
 * <p>
 * Created by hanwen on 15-12-16.
 */
public interface ModelFactory {

  /**
   * initial one row present model to access cell value.
   *
   * @param row
   * @return
   */
  Object create(ExcelRow row);
}

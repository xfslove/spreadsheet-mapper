package me.excel.tools.processor;

import me.excel.tools.model.excel.Row;

/**
 * model factory
 * <p>
 * Created by hanwen on 15-12-16.
 */
public interface ObjectFactory {

  /**
   * initial one row present object to access cell value.
   *
   * @param row which row
   * @return initialized object
   */
  Object create(Row row);

  /**
   * @return which sheet this object factory matched
   */
  int getSheetIndex();
}

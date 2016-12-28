package me.excel.tools.factory;

import me.excel.tools.model.excel.Row;

/**
 * model factory
 * <p>
 * Created by hanwen on 15-12-16.
 */
public interface ModelFactory<OBJECT> {

  /**
   * initial one row present object to access cell value.
   *
   * @param row which row
   * @return initialized object
   */
  OBJECT create(Row row);
}

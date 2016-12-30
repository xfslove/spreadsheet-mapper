package excel.engine.w2o.processor;

import excel.engine.model.core.Row;

/**
 * model template
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
}

package spread.sheet.w2o.processor;

import spread.sheet.model.core.Row;

/**
 * object factory
 * <p>
 * Created by hanwen on 15-12-16.
 */
public interface ObjectFactory<T> {

  /**
   * initial one row present object to access cell value.
   *
   * @param row which row
   * @return initialized object
   */
  T create(Row row);
}

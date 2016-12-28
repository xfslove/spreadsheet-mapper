package me.excel.tools.setter;


import me.excel.tools.model.excel.Cell;

/**
 * object field value setter
 * <p>
 * Created by hanwen on 15-12-16.
 */
public interface FieldValueSetter {

  /**
   * set object field from cell value
   *
   * @param data      supplied object
   * @param cell cell
   */
  void set(Object data, Cell cell);

  /**
   * @return which field this setter matched
   */
  String getMatchField();
}

package excel.engine.importer.setter;


import excel.engine.model.excel.Cell;

/**
 * object field value setter
 * <p>
 * Created by hanwen on 15-12-16.
 */
public interface FieldValueSetter {

  /**
   * set object field from cell value
   *
   * @param data supplied object
   * @param cell cell
   */
  void set(Object data, Cell cell);

  /**
   * @return which field this setter matched
   */
  String getMatchField();

  /**
   * @return which sheet this setter matched
   */
  int getSheetIndex();
}

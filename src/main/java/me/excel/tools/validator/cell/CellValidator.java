package me.excel.tools.validator.cell;


import me.excel.tools.model.excel.Cell;
import me.excel.tools.validator.DataValidator;

/**
 * excel cell value validator, after row validators.
 * <p>
 * Created by hanwen on 15-12-15.
 */
public interface CellValidator extends DataValidator<Cell> {

  /**
   * valid supplied excel cell value
   *
   * @param cell cell
   * @return result
   */
  @Override
  boolean valid(Cell cell);

  /**
   * @return which field this validator to valid
   */
  String getMatchField();

  /**
   * @return which sheet this validator matched
   */
  int getSheetIndex();
}

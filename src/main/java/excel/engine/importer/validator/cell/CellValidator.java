package excel.engine.importer.validator.cell;


import excel.engine.importer.validator.RelationValidator;
import excel.engine.model.excel.Cell;

/**
 * excel cell value validator, after row validators.
 * <p>
 * Created by hanwen on 15-12-15.
 */
public interface CellValidator extends RelationValidator<Cell> {

  /**
   * valid supplied excel cell value
   *
   * @param cell cell
   * @return true if pass
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

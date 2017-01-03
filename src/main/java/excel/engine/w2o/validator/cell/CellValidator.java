package excel.engine.w2o.validator.cell;


import excel.engine.model.core.Cell;
import excel.engine.model.meta.FieldMeta;
import excel.engine.w2o.validator.RelationValidator;

/**
 * cell value validator, general(no dependency) after row validators.
 * <p>
 * Created by hanwen on 15-12-15.
 */
public interface CellValidator extends RelationValidator {

  /**
   * valid supplied excel cell value
   *
   * @param cell      cell
   * @param fieldMeta field meta
   * @return true if pass
   */
  boolean valid(Cell cell, FieldMeta fieldMeta);

  /**
   * @return which field this validator to valid
   */
  String getMatchField();

  /**
   * @return message on which field
   */
  String getMessageOnField();
}

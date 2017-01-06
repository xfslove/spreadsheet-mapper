package spreadsheet.mapper.w2o.validator.cell;


import spreadsheet.mapper.model.meta.FieldMeta;
import spreadsheet.mapper.model.core.Cell;
import spreadsheet.mapper.w2o.validator.DependencyValidator;

/**
 * cell value validator, general(no dependency) after row validators.
 * <p>
 * Created by hanwen on 15-12-15.
 */
public interface CellValidator extends DependencyValidator {

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

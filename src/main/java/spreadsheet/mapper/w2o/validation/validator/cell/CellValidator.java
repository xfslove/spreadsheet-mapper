package spreadsheet.mapper.w2o.validation.validator.cell;


import spreadsheet.mapper.model.core.Cell;
import spreadsheet.mapper.model.meta.FieldMeta;
import spreadsheet.mapper.w2o.validation.validator.DependencyValidator;

/**
 * cell value validator, general(no dependency) after row validators.
 * <p>
 * Created by hanwen on 15-12-15.
 */
public interface CellValidator extends DependencyValidator {

  /**
   * valid supplied cell
   *
   * @param cell      {@link Cell}
   * @param fieldMeta {@link FieldMeta}
   * @return true if pass
   */
  boolean valid(Cell cell, FieldMeta fieldMeta);

  /**
   * @return which field this validator to valid
   * @see FieldMeta#getName()
   */
  String getMatchField();

  /**
   * @return message on which field
   * @see FieldMeta#getName()
   */
  String getMessageOnField();
}

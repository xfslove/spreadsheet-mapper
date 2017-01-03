package extensible.sheet.w2o.validator;

import extensible.sheet.model.core.Sheet;
import extensible.sheet.model.message.ErrorMessage;
import extensible.sheet.model.meta.SheetMeta;
import extensible.sheet.w2o.validator.cell.CellValidator;
import extensible.sheet.w2o.validator.row.RowValidator;
import extensible.sheet.w2o.validator.sheet.SheetValidator;
import extensible.sheet.model.message.MessageWriteStrategies;

import java.util.List;

/**
 * sheet validate engine
 * <p>
 * Created by hanwen on 15-12-16.
 */
public interface SheetValidateEngine {

  /**
   * @param validators sheet validator
   * @see SheetValidator
   */
  SheetValidateEngine sheetValidator(SheetValidator... validators);

  /**
   * @param validators row validator
   * @see RowValidator
   * @see RelationValidator#getGroup()
   * @see RelationValidator#getDependsOn()
   */
  SheetValidateEngine rowValidator(RowValidator... validators);

  /**
   * @param validators cell validator
   * @see CellValidator
   * @see RelationValidator#getGroup()
   * @see RelationValidator#getDependsOn()
   */
  SheetValidateEngine cellValidator(CellValidator... validators);

  /**
   * @param sheet sheet
   */
  SheetValidateEngine sheet(Sheet sheet);

  /**
   * @param sheetMeta sheet meta
   */
  SheetValidateEngine sheetMeta(SheetMeta sheetMeta);

  /**
   * execute valid
   *
   * @return true if passed all validators
   */
  boolean valid();

  /**
   * <pre>
   * message write strategy of {@link SheetValidator#getErrorMessage()} is {@link MessageWriteStrategies#TEXT_BOX}
   * message write strategy of {@link RowValidator#getErrorMessage()} &amp; {@link CellValidator#getErrorMessage()} is {@link MessageWriteStrategies#COMMENT}
   * </pre>
   *
   * @return list of valid error messages
   */
  List<ErrorMessage> getErrorMessages();
}

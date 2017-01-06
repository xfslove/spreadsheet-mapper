package spread.sheet.w2o.validator;

import spread.sheet.model.core.Sheet;
import spread.sheet.model.message.ErrorMessage;
import spread.sheet.model.message.MessageWriteStrategies;
import spread.sheet.model.meta.SheetMeta;
import spread.sheet.w2o.validator.cell.CellValidator;
import spread.sheet.w2o.validator.row.RowValidator;
import spread.sheet.w2o.validator.sheet.SheetValidator;

import java.util.List;

/**
 * sheet validate engine
 * <p>
 * Created by hanwen on 15-12-16.
 */
public interface SheetValidateHelper {

  /**
   * @param validators sheet validator
   * @see SheetValidator
   */
  SheetValidateHelper sheetValidator(SheetValidator... validators);

  /**
   * @param validators row validator
   * @see RowValidator
   * @see DependencyValidator#getGroup()
   * @see DependencyValidator#getDependsOn()
   */
  SheetValidateHelper rowValidator(RowValidator... validators);

  /**
   * @param validators cell validator
   * @see CellValidator
   * @see DependencyValidator#getGroup()
   * @see DependencyValidator#getDependsOn()
   */
  SheetValidateHelper cellValidator(CellValidator... validators);

  /**
   * @param sheet sheet
   */
  SheetValidateHelper sheet(Sheet sheet);

  /**
   * @param sheetMeta sheet meta
   */
  SheetValidateHelper sheetMeta(SheetMeta sheetMeta);

  /**
   * execute valid
   *
   * @return true if pass all
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

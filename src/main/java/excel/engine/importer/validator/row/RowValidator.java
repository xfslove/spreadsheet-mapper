package excel.engine.importer.validator.row;


import excel.engine.importer.validator.RelationValidator;
import excel.engine.model.excel.Row;

/**
 * excel row values validator, after sheet validators.
 * <p>
 * Created by hanwen on 4/26/16.
 */
public interface RowValidator extends RelationValidator<Row> {

  /**
   * valid supplied excel row
   *
   * @param row row
   * @return true if pass
   */
  @Override
  boolean valid(Row row);

  /**
   * @return which sheet this validator belong to
   */
  int getSheetIndex();
}

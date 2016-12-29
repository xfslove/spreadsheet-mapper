package java.excel.engine.importer.setter;

import java.excel.engine.BooleanTranslator;
import java.excel.engine.exception.ExcelProcessException;
import java.excel.engine.model.excel.Cell;
import org.apache.commons.beanutils.BeanUtils;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.excel.engine.util.FieldUtils;

/**
 * boolean field value setter
 * <p>
 * Created by hanwen on 5/3/16.
 */
public class BooleanValueSetter extends FieldValueSetterAdapter {

  private static final Logger LOGGER = LoggerFactory.getLogger(BooleanValueSetter.class);

  public BooleanValueSetter(String matchField) {
    super(matchField);
  }

  @Override
  public void set(Object data, Cell cell) {
    try {
      BeanUtils.setProperty(data, FieldUtils.detectRealField(getMatchField()), BooleanTranslator.parseBoolean(cell.getValue()));
    } catch (Exception e) {
      LOGGER.error(ExceptionUtils.getStackTrace(e));
      throw new ExcelProcessException(e);
    }
  }
}

package excel.engine.o2w.extractor;


import excel.engine.model.meta.FieldMeta;
import excel.engine.util.BooleanUtils;
import excel.engine.util.FieldUtils;
import excel.engine.w2o.processor.ExcelProcessException;
import org.apache.commons.beanutils.NestedNullException;
import org.apache.commons.beanutils.PropertyUtils;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * chinese boolean readable value extractor
 * <p>
 * Created by hanwen on 16/3/18.
 */
public class BooleanZhExtractor extends FieldValueExtractorAdapter {

  private static final Logger LOGGER = LoggerFactory.getLogger(BooleanZhExtractor.class);

  public BooleanZhExtractor(String matchField) {
    super(matchField);
  }

  @Override
  public String getStringValue(Object data, FieldMeta fieldMeta) {

    try {
      Object value = PropertyUtils.getProperty(data, FieldUtils.detectRealField(fieldMeta.getName()));

      return BooleanUtils.booleanToZhString(value);
    } catch (NestedNullException e) {
      LOGGER.trace(ExceptionUtils.getStackTrace(e));
      return null;
    } catch (Exception e) {
      LOGGER.error(ExceptionUtils.getStackTrace(e));
      throw new ExcelProcessException(e);
    }
  }
}

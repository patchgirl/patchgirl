module ScriptContext where

import           Interpolator
import qualified PatchGirl.Web.ScenarioNode.Model as Web


data ScriptContext = ScriptContext
    { sceneVars       :: Web.SceneVariables
    , environmentVars :: EnvironmentVars
    , globalVars      :: ScenarioVars
    , localVars       :: ScenarioVars
    }
